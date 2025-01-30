#include <assert.h>
#include <string.h>

void f211(const char* s) {
    assert(s[0] == 'a');
    assert(s[1] == 'b');
    assert(strlen(s) == 2);
}
