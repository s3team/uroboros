#include <stdio.h>
#include <string.h>
#include <unistd.h>

void print_filename(void) {
    char buf[256];
    ssize_t n = readlink("/proc/self/exe", buf, sizeof(buf) - 1);
    if (n <= 0) return;
    buf[n] = '\0';
    const char *name = strrchr(buf, '/');
    printf("filename: %s\n", name ? name + 1 : buf);
}
