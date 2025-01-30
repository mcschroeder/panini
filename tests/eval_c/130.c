#include <string.h>
#include <assert.h>

void f130(const char* s) {
    assert(s[strlen(s) - 1] == 'a');
}
