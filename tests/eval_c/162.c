#include <assert.h>
#include <string.h>

void f162(const char* s) {
    assert(strlen(s) <= 1);
    assert(strchr(s, 'a') == NULL);
}
