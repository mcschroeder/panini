#include <assert.h>
#include <string.h>

void f523(const char *s) {
    assert(strncmp(s, "aa", 2) == 0);
}
