#include <assert.h>
#include <string.h>

void f212(const char* s) {
    assert(strchr(s, 'a') == s);
    assert(strchr(s, 'b') == s + 1);
    assert(strlen(s) == 2);
}
