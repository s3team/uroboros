#include <stdio.h>


void print_int_arg (int x) {
  printf("int arg: %d\n", x);
}

void print_char_star_arg (char* x) {
  printf("char* arg: %s\n", x);
}

void print_int_star_arg (int* x) {
  printf("int* arg: %d\n", *x);
}

