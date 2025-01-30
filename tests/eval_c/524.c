#include <assert.h>
#include <string.h>

void f524(const char *s) {
    char a1 = s[0];
    char a2 = s[1];
    assert(a1 == 'a');
    assert(a2 == 'a');
}
