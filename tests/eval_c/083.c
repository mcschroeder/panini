#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void f083(const char *s) {
    if (s[0] == 'a') {
        assert(strlen(s) == 1);
    } else {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
}
