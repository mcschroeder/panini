#include <string.h>
#include <assert.h>

void f323(const char* s) {
    char a = s[0];
    char x = s[1];
    char y = s[1];
    char b = s[2];
    assert(a == 'a');
    assert(b == 'b');
}
