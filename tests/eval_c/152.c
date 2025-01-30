#include <string.h>
#include <assert.h>

void f152(const char* s) {
    assert(strlen(s) == 1);
    assert(strchr(s, 'a') == NULL);
}
