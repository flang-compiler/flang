
#include <stdio.h>
#include <string.h>

extern void check(int* res, int* exp, int* np);

void my_printing(char * to_print) {
    printf("%s\n", to_print);
    int one[1] = {1};
    int zero[1] = {0};
    if (strcmp(to_print, "Something to print") == 0 ) {
        check(one, one, one);
    } else {
        check(zero, one, one);
    }
}

