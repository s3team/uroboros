/* This program needs to be compiled with -D_GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HAVE_DECL_PROGRAM_INVOCATION_NAME 1

const char *program_name = NULL;

int main(int argc, char **argv){
	program_name = argv[0];
	printf("program_name:%s\n",program_name);
#if HAVE_DECL_PROGRAM_INVOCATION_NAME
  	program_invocation_name = (char *) argv[0];
	printf("program_invocation_name:%s\n",program_invocation_name);
#endif
	return 0;
}
