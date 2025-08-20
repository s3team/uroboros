/*
 * fun.c
 *
 * Utility functions for instrumentation examples in point_examples/.
 */
#include <stdio.h>

int CALLED = 1;

void log_execution_times()
{
  printf("called %d times\n", CALLED);
  CALLED++;
}

void print_args(const char *format, int x)
{
  printf("^passed: %s %d^\n", format, x);
}

void print_addr(void *ptr)
{
  printf("addr: %p\n", (void *)ptr);
}

void print_string(char *x)
{
  printf("passed string: %s\n", x);
}

void print_int(int x)
{
  printf("passed int: %d\n", x);
}

void before_print_info()
{
  puts("before call to print_info");
}