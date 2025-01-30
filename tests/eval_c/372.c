#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

bool f372(const char *s) {
    if (strcmp(s, "a") == 0) {
        return true;
    } else if (strcmp(s, "b") == 0) {
        return false;
    } else {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
}
