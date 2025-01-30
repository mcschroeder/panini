#include <string.h>
#include <assert.h>

void f091(const char* s) {
    assert(strcmp(s, "") == 0 || strcmp(s, "a") == 0);
}
