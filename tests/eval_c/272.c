#include <assert.h>
#include <string.h>

void f272(const char* s) {
    char* bi = strchr(s, 'b');
    if (bi == s + 1) {
        assert(s[0] == 'a');
        assert(strlen(s) == 2);
    } else {
        assert(strcmp(s, "a") == 0);
    }
}
