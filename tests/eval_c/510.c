#include <assert.h>
#include <string.h>

void f510(const char *s) {
    assert(strcmp(s, "abc") == 0 || strcmp(s, "ab") == 0 || strcmp(s, "a") == 0 ||
           strcmp(s, "ac") == 0 || strcmp(s, "bc") == 0 || strcmp(s, "b") == 0 ||
           strcmp(s, "c") == 0 || strcmp(s, "") == 0);
}
