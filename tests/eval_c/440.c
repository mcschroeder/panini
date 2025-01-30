#include <assert.h>
#include <string.h>

void f440(const char *s) {
    assert(strcmp(s, "ab") == 0 || strcmp(s, "c") == 0);
}
