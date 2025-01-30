#include <assert.h>
#include <string.h>

void f530(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] == 'a') {
            assert(s[i + 1] == 'b');
            i += 2;
        } else {
            i++;
        }
    }
}
