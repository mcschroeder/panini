#include <string.h>
#include <assert.h>

const char* f094(const char* s) {
    if (strlen(s) > 0) {
        char t[2];
        t[0] = s[0];
        t[1] = '\0';
        assert(strcmp(t, "a") == 0);
        assert(strlen(s) == 1);
        return t;
    } else {
        return s;
    }
}
