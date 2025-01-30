#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void f421(const char *s) {
    assert(strlen(s) == 1);
    if (s[0] == 'a') {
        return;
    }
    if (s[0] == 'b') {
        return;
    }
    if (s[0] == 'c') {
        return;
    }
    fprintf(stderr, "Exception\n");
    exit(1);
}
