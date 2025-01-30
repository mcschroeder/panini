#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

char f085(const char *s) {
    if (strlen(s) != 1) {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
    assert(s[0] == 'a');
    return s[0];
}
