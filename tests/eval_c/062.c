#include <assert.h>
#include <string.h>

void f062(const char *s) {
    int i = 0;
    char c0 = s[i];
    char c1 = s[i + 1];
    char c2 = s[i + 2];
    assert(strlen(s) == i + 3);
}
