#include <assert.h>
#include <string.h>

void f065(const char *s) {
    int i = strlen(s) - 1;
    assert(i == 2);
    while (i >= 0) {
        char c = s[i];
        i -= 1;
    }
}
