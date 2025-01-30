#include <assert.h>
#include <string.h>

char f041(const char *s) {
    assert(strlen(s) <= 2);
    return s[0] + s[1];
}
