#include <assert.h>
#include <string.h>

void f183(const char* s) {
    assert(s[strlen(s) - 1] == 'a');
    assert(s[strlen(s) - 2] != 'a');
    assert(strlen(s) == 2);
}
