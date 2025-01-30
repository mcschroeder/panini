#include <assert.h>
#include <string.h>

void f292(const char* s) {
    if (strlen(s) == 0) {
        return;
    }

    char* bi = strchr(s, 'b');
    if (bi == s) {
        assert(strlen(s) == 1);
    } else if (bi == s + 1) {
        assert(s[0] == 'a');
        assert(strlen(s) == 2);
    } else {
        assert(strcmp(s, "a") == 0);
    }
}
