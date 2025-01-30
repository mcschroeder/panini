#include <string.h>
#include <stdlib.h>
#include <assert.h>

void f154(const char* s) {
    if (s[0] == 'a') {
        exit(1);
    } else {
        assert(strlen(s) == 1);
    }
}
