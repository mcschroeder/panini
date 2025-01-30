#include <assert.h>
#include <string.h>

void f161(const char* s) {
    assert((strcmp(s, "") == 0 || strcmp(s, "a") != 0) && strlen(s) <= 1);
}
