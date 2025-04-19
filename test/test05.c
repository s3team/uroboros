#include <stdio.h>

struct info
{
	char* name;
	int age;
	char gender;
};

struct info const persons[] = {
	{"Jinquan Zhang", 26, 'm'},
	{"Rui Zhong", 26, 'm'},
	{"Zihao Wang", 27, 'm'}
};

void print_info(struct info const *persons){
	struct info pi = *persons;
	printf("name: %s\n", pi.name);
	printf("age: %d\n", pi.age);
	printf("gender: %c\n", pi.gender);
}

int main() {
	print_info(persons);
	return 0;
}
