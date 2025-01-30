#include <string.h>
#include <assert.h>

void f092(const char* s) {
    assert(strlen(s) == 0 || strcmp(s, "a") == 0);
}
