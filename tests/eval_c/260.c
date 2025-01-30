#include <assert.h>
#include <string.h>

void f260(const char* s) {
    assert(strcmp(s, "b") == 0 || strcmp(s, "ab") == 0);
}
