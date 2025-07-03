#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>

#include "common.h"

FileList* decrypt_file(FileList* encr) {
    FileList *file = newfile();

    struct AES_ctx ctx;
    AES_init_ctx_iv(&ctx, encr->key, encr->iv);

    uint8_t buf[CHUNKSIZE];
    
    FILE *f = fopen(encr->path, "rb+");
    while (1) {
        int len = fread(buf, 1, CHUNKSIZE, f);
        if (len <= 0)
            break;

        fseek(f, -len, SEEK_CUR);
        AES_CTR_xcrypt_buffer(&ctx, buf, len);
        int wlen = fwrite(buf, 1, len, f);
    }
    fclose(f);

    strncpy(file->path, encr->path, PATHLEN);
    file->path[strlen(file->path) - 6] = '\0';
    rename(encr->path, file->path);

    return file;
}

int main(int argc, char** argv) {
    if (argc != 3) {
        printf("Provide info file and unlock key!\nUsage: ./unrware <info-file> <unlock-key>\n");
        return EXIT_FAILURE;
    }

    uint8_t secret[ECC_PUB_KEY_SIZE];
    // Read the unlock secret
    hextoarr(argv[2], secret);

    struct AES_ctx infoenc;
    AES_init_ctx_iv(&infoenc, secret, &secret[ECC_PRV_KEY_SIZE]);

    FileList *encr = (FileList *) malloc(sizeof(FileList));
    FILE *info = fopen(argv[1], "rb");
    fseek(info, ECC_PUB_KEY_SIZE, SEEK_SET); // Skip first ECC_PUB_KEY_SIZE bytes of public key
    while (1) {
        int len, n;
        n = fread(&len, 1, sizeof(int), info);
        if (n != sizeof(int) || len <= 0 || len >= PATHLEN)
            break;
        n = fread(encr->path, 1, len, info);
        if (n <= 0)
            break;
        // Read key and iv
        n = fread(encr->key, 1, AES_KEYLEN + AES_BLOCKLEN, info);
        if (n <= 0)
            break;

        // Decrypt the key and iv
        AES_CTR_xcrypt_buffer(&infoenc, encr->key, AES_KEYLEN + AES_BLOCKLEN);
        
        // Decrypt the file
        printf("File: %s\n", encr->path);
        decrypt_file(encr);
    }
    fclose(info);

    return EXIT_SUCCESS;
}
