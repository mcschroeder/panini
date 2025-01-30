#include <assert.h>
#include <string.h>

void f360(const char *s) {
    assert(s[0] == 'a');
    assert(s[strlen(s) - 1] == 'b');
}
