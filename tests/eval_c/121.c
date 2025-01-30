#include <string.h>
#include <assert.h>

void f121(const char* s) {
    assert(strncmp(s, "a", 1) == 0);
}
