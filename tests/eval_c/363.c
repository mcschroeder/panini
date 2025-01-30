#include <assert.h>
#include <string.h>

void f363(const char *s) {
    assert(strchr(s, 'a') == &s[0]);
    assert(strrchr(s, 'b') == &s[strlen(s) - 1]);
}
