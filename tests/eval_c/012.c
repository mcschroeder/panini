#include <assert.h>
#include <string.h>

char f012(const char *s) {
    size_t n = strlen(s);
    assert(n <= 1);
    return s[n - 1];
}
