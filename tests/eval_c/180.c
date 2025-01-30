#include <assert.h>
#include <string.h>

void f180(const char* s) {
    assert(s[0] != 'a');
    assert(s[1] == 'a');
    assert(strlen(s) == 2);
}
