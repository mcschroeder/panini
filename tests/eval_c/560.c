#include <assert.h>
#include <string.h>

void f560(const char *s) {
    char *bi = strchr(s, 'b');
    if (bi == s + 1) {
        assert(s[0] == 'a');
    } else {
        assert(bi == s);
    }
}