#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void f042(const char *s) {
    size_t n = strlen(s);
    if (n > 2) {
        fprintf(stderr, "Exception\n");
        exit(1);
    } else {
        char c1 = s[n - 1];
        char c2 = s[n - 2];
        return;
    }
}
