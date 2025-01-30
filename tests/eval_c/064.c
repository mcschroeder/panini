#include <assert.h>
#include <string.h>

void f064(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        char c = s[i];
        i += 1;
    }
    assert(i == 3);
}
