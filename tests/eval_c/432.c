#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int f432(const char *s) {
    if (s[0] == 'b') {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
    
    size_t n = strlen(s);
    if (s[n - 1] == 'a' || s[n - 1] == 'b') {
        if (n > 1) {
            fprintf(stderr, "Exception\n");
            exit(1);
        }
        return 1;
    } else {
        assert(n == 1);
        return 0;
    }
}
