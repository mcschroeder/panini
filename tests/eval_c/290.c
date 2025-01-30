#include <assert.h>
#include <string.h>

void f290(const char* s) {
    assert(strcmp(s, "") == 0 || strcmp(s, "a") == 0 || strcmp(s, "b") == 0 || strcmp(s, "ab") == 0);
}
