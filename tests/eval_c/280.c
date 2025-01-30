#include <assert.h>
#include <string.h>

void f280(const char* s) {
    assert(strcmp(s, "") == 0 || strcmp(s, "ab") == 0);
}
