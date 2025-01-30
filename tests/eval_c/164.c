#include <assert.h>
#include <string.h>
#include <stdlib.h>

void f164(const char* s) {
    if (strlen(s) == 0) {
        return;
    } else if (s[0] == 'a') {
        exit(1);
    } else {
        assert(strlen(s) == 1);
    }
}
