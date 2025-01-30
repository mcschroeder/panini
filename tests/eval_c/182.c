#include <assert.h>
#include <string.h>
#include <stdlib.h>

void f182(const char* s) {
    if (s[0] == 'a') {
        exit(1);
    } else if (s[1] == 'a') {
        assert(strlen(s) == 2);
    } else {
        exit(1);
    }
}
