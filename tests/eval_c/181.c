#include <assert.h>
#include <string.h>

void f181(const char* s) {
    char x = s[0];
    char y = s[1];
    assert(x != 'a');
    assert(y == 'a');
}
