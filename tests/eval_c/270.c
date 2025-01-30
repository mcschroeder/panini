#include <assert.h>
#include <string.h>

void f270(const char* s) {
    assert(strcmp(s, "a") == 0 || strcmp(s, "ab") == 0);
}
