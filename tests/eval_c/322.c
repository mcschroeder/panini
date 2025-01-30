#include <string.h>
#include <assert.h>

void f322(const char* s) {
    char a = s[0];
    char x = s[1];
    char b = s[2];
    assert(a == 'a');
    assert(b == 'b');
}
