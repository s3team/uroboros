#include <stdio.h>
#include <string.h>

int main(int argc, char **argv){
	int u = 0;
	switch(argc) {
	default:
		break;
	case 4:
		--u;
	case 1:
		++u;
	case 2:
		--u;
	case 3:
		--u;
	case 0:
		++u;
	case 5:
		++u;
	case 6:
		--u;
	case 7:
		--u;
	case 8:
		++u;
	}
	printf("result of u is %d\n", u);
	return 0;
}
