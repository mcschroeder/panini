#include <assert.h>
#include <string.h>

void f013(const char *s) {
    char t[2];
    strncpy(t, s, 1);
    t[1] = '\0';
    assert(strlen(t) <= 1);
}
