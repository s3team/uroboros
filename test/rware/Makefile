#:Compiler and flags
CC = gcc
#CFLAGS = -O3 -no-pie -static -m32 -I./tiny-AES-c -I./tiny-ECDH-c
CFLAGS = -O3 -no-pie -static -I./tiny-AES-c -I./tiny-ECDH-c

# Source files
GENECDH_SRC = src/genecdh.c tiny-ECDH-c/ecdh.c
ENCRYPTOR_SRC = src/rware.c tiny-AES-c/aes.c tiny-ECDH-c/ecdh.c
DECRYPTOR_SRC = src/unrware.c tiny-AES-c/aes.c
KEYDIR = keys/

# Targets
GENECDH = build/genecdh
ENCRYPTOR = build/rware
DECRYPTOR = build/unrware

# Default target
all: $(ENCRYPTOR) $(DECRYPTOR) $(GENECDH)

genkey: $(GENECDH)
	$(GENECDH) genkey
	@mkdir -p $(KEYDIR)
	mv private.key $(KEYDIR)
	mv public_key.h $(KEYDIR)

# Compile genecdh and generate public key
$(GENECDH): $(GENECDH_SRC)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -o $@ $^

# Compile encryptor with public key
$(ENCRYPTOR): $(ENCRYPTOR_SRC) $(KEY_HDR)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -o $@ $(ENCRYPTOR_SRC) -I$(KEYDIR)

# Compile decryptor
$(DECRYPTOR): $(DECRYPTOR_SRC)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -o $@ $^

# Clean up generated files
clean:
	rm -f $(GENECDH) $(ENCRYPTOR) $(DECRYPTOR)
	rm -rf hello
	rm -rf hello2
	cp -R hello-bk hello
	cp -R hello-bk hello2

.PHONY: all clean genkey

