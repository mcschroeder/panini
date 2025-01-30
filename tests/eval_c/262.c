#include <assert.h>
#include <string.h>

void f262(const char* s) {
    char* bi = strchr(s, 'b');
    if (bi == s + 1) {
        assert(s[0] == 'a');
        assert(strlen(s) == 2);
    } else {
        assert(bi == s);
        assert(strlen(s) == 1);
    }
}
