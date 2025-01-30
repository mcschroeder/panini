#include <string.h>
#include <assert.h>

void f311(const char* s) {
    assert(strncmp(s + strlen(s) - 2, "ab", 2) == 0);
}
