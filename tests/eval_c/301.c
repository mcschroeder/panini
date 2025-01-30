#include <string.h>
#include <assert.h>

void f301(const char* s) {
    assert(strncmp(s, "ab", 2) == 0);
}
