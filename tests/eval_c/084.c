#include <assert.h>
#include <string.h>

void f084(const char *s) {
    char t[2];
    strncpy(t, s, 1);
    t[1] = '\0';

    assert(strcmp(t, "a") == 0);
    assert(strlen(s) == 1);
}
