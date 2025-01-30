#include <assert.h>
#include <string.h>

void f082(const char *s) {
    if (s[0] == 'a') {
        assert(strlen(s) == 1);
    } else {
        assert(0);
    }
}
