#include <assert.h>
#include <string.h>

void f431(const char *s) {
    assert(strlen(s) == 1);
    char c = s[0];
    if (c != 'a') {
        if (c != 'c') {
            assert(c != 'b');
        }
    }
}
