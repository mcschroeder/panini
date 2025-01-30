#include <assert.h>
#include <string.h>

char f011(const char *s) {
    assert(strlen(s) <= 1);
    return s[0];
}
