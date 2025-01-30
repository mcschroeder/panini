#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void f002(const char *s) {
    if (strcmp(s, "") != 0) {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
}
