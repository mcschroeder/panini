#include <assert.h>
#include <string.h>

void f441(const char *s) {
    if (s[0] == 'a') {
        assert(s[1] == 'b');
        assert(strlen(s) <= 2);
    } else {
        assert(s[0] == 'c');
        assert(strlen(s) == 1);
    }
}
