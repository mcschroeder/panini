#include <assert.h>
#include <string.h>

void f063(const char *s) {
    int i = 0;
    while (i < 3) {
        char c = s[i];
        i += 1;
    }
    assert(strlen(s) == i);
}
