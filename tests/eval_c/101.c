#include <string.h>
#include <assert.h>

void f101(const char* s) {
    assert(strlen(s) <= 2);
    assert(s[0] == 'a');
    assert(s[1] == 'a');
}
