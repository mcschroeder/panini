#include <assert.h>
#include <string.h>

void f540(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] == 'a') {
            assert(s[i + 1] == 'b');
            assert(s[i + 2] == 'b');
            i += 3;
        } else {
            i++;
        }
    }
}
