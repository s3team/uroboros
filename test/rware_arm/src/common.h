#ifndef __COMMON_H
#define __COMMON_H

#define PATHLEN 1024
#define CHUNKSIZE 512

#include "aes.h"
#include "ecdh.h"

struct FileList {
    char path[PATHLEN];
    uint8_t key[AES_KEYLEN]; // AES_KEYLEN
    uint8_t iv[AES_BLOCKLEN]; // AES_BLOCKLEN
    struct FileList *next;
};
typedef struct FileList FileList;

FileList* newfile() {
    FileList *n = (FileList *) malloc(sizeof(FileList));
    n->next = NULL;
    n->path[0] = '\0';
    return n;
}

void freefilelist(FileList *list) {
    while (list != NULL) {
        FileList *tmp = list->next;
        free(list);
        list = tmp;
    }
}

void hextoarr(const char *s, uint8_t *arr) {
    uint8_t v, t;

    for (int i=0; s[i] != '\0'; i++) {
        if (s[i] >= '0' && s[i] <= '9')
            t = s[i] - '0';
        else if (s[i] >= 'a' && s[i] <= 'f')
            t = (s[i] - 'a') + 10;
        else if (s[i] >= 'A' && s[i] <= 'F')
            t = (s[i] - 'A') + 10;

        if (i & 1) {
            arr[i/2] = v+t;
            v = 0;
        } else {
            v = t << 4;
        }
    }
}

#endif
