#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char f015(const char *s) {
    if (strlen(s) != 1) {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
    return s[0];
}