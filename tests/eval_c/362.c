#include <assert.h>
#include <string.h>

void f362(const char *s) {
    int i = 0;
    assert(s[i] == 'a');
    while (i < strlen(s) - 1) {
        i += 1;
    }
    assert(s[i] == 'b');
}
