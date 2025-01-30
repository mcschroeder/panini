#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void f371(const char *s) {
    assert(strlen(s) == 1);
    if (s[0] == 'a') {
        return;
    } else if (s[0] == 'b') {
        return;
    }
    fprintf(stderr, "Exception\n");
    exit(1);
}
