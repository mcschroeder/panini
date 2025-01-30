#include <assert.h>
#include <string.h>

void f081(const char *s) {
    assert(s[0] == 'a');
    assert(strlen(s) == 1);
}
