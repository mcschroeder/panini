#include <assert.h>
#include <string.h>

void f044(const char *s) {
    size_t n = strlen(s) - 2;
    char c1 = s[n];
    char c2 = s[n + 1];
    assert(n == 0);
}
