#include <assert.h>
#include <string.h>

void f370(const char *s) {
    assert(strcmp(s, "a") == 0 || strcmp(s, "b") == 0);
}
