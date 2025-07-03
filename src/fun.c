#include <stdio.h>


int CALLED = 1;

void before_print_info () {
  puts("before call to print_info");  
}

void log_execution_times () {
  printf("called %d times\n", CALLED);  
  CALLED ++;
}

void print_args (const char *format, int x) {
  printf("^passed: %s %d^\n", format, x);  
}

int print_string2 (char* x) {
  printf("passed string: %s\n", x);
  int len = strlen(x);
  return len;
}

void print_string (char* x) {
  printf("passed string: %s\n", x);
}

void print_int (int x) {
  printf("passed int: %d\n", x);
}

void print_int2 (int a, int b) {
  printf("passed int: %d %d\n", a, b);
}

void print_int3 (int a, int b, int c) {
  printf("passed int: %d %d %d\n", a, b, c);
}
