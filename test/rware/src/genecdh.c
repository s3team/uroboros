#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <sys/random.h>

#include "common.h"

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Pass command: genkey|shared\n");
        return EXIT_FAILURE;
    }
    if (strcmp(argv[1], "genkey") == 0) {
        uint8_t priv[ECC_PRV_KEY_SIZE];
        uint8_t pub[ECC_PUB_KEY_SIZE];

        getrandom(priv, ECC_PRV_KEY_SIZE, 0);
        ecdh_generate_keys(pub, priv);
        
        FILE *privf = fopen("private.key", "w");
        for (int i=0; i <  ECC_PRV_KEY_SIZE; i++)
            fprintf(privf, "%02x", priv[i]);
        fclose(privf);

        FILE *pubf = fopen("public_key.h", "w");
        fprintf(pubf, "uint8_t Apub[ECC_PUB_KEY_SIZE] = {\n");
        for (int i=0; i <  ECC_PUB_KEY_SIZE; i++)
            fprintf(pubf, "0x%02x, ", pub[i]);
        fprintf(pubf, "\n};\n");
        fclose(pubf);
    } else if (strcmp(argv[1], "shared") == 0) {
        if (argc != 4) {
            fprintf(stderr, "For shared, pass keys: <A-private> <B-public>\n");
            return EXIT_FAILURE;
        }
        uint8_t apriv[ECC_PRV_KEY_SIZE];
        uint8_t bpub[ECC_PUB_KEY_SIZE];
        uint8_t shared[ECC_PUB_KEY_SIZE];
        hextoarr(argv[2], apriv);
        hextoarr(argv[3], bpub);

        ecdh_shared_secret(apriv, bpub, shared);
        for (int i=0; i < ECC_PUB_KEY_SIZE; i++)
            printf("%02x", shared[i]);
        printf("\n");
    } else {
        fprintf(stderr, "Pass command: genkey|shared\n");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
