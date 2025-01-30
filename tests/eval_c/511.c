#include <assert.h>
#include <string.h>

void f511(const char *s) {
    if (strcmp(s, "abc") == 0) {
        return;
    } else if (strcmp(s, "ab") == 0) {
        return;
    } else if (strcmp(s, "a") == 0) {
        return;
    } else if (strcmp(s, "ac") == 0) {
        return;
    } else if (strcmp(s, "bc") == 0) {
        return;
    } else if (strcmp(s, "b") == 0) {
        return;
    } else if (strcmp(s, "c") == 0) {
        return;
    } else {
        assert(strcmp(s, "") == 0);
    }
}
