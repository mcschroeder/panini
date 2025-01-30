#include <assert.h>
#include <string.h>

void f390(const char *s) {
    assert(strcmp(s, "a") == 0 || strcmp(s, "b") == 0 || strlen(s) == 0);
}
