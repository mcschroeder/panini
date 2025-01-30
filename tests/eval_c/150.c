#include <string.h>
#include <assert.h>

void f150(const char* s) {
    assert(strlen(s) == 1);
    assert(s[0] != 'a');
}
