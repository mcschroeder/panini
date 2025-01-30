#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char f021(const char *s) {
    if (strlen(s) == 1) {
        return s[0];
    } else if (strlen(s) == 0) {
        return 'a';
    } else {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
}