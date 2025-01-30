#include <string.h>
#include <assert.h>

void f320(const char* s) {
    assert(s[0] == 'a');
    assert(s[2] == 'b');
    assert(strlen(s) == 3);
}
