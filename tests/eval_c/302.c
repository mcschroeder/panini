#include <string.h>
#include <assert.h>

void f302(const char* s) {
    char a = s[0];
    char b = s[1];
    assert(a == 'a');
    assert(b == 'b');
}
