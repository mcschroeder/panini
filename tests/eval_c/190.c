#include <assert.h>
#include <string.h>

void f190(const char* s) {
    assert(strrchr(s, 'a') == s + strlen(s) - 1);
}
