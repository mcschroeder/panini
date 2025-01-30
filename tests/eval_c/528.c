#include <assert.h>
#include <string.h>

void f528(const char *s) {
    char a1 = s[0];
    char a2 = s[1];
    assert(a1 == a2);
    assert(s[1] == 'a');
}
