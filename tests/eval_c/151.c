#include <string.h>
#include <assert.h>

void f151(const char* s) {
    assert(strlen(s) == 1);
    assert(strcmp(s, "a") != 0);
}
