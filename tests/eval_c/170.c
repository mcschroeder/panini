#include <assert.h>
#include <string.h>

void f170(const char* s) {
    assert(strchr(s, 'a') == NULL);
}
