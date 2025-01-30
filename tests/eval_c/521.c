#include <assert.h>

void f521(const char *s) {
    assert(s[0] == 'a');
    assert(s[1] == s[0]);
}
