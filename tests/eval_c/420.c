#include <assert.h>
#include <string.h>

void f420(const char *s) {
    assert(strcmp(s, "a") == 0 || strcmp(s, "b") == 0 || strcmp(s, "c") == 0);
}
