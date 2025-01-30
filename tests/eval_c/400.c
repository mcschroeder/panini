#include <assert.h>
#include <string.h>

void f400(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] != 'a') {
            break;
        }
        i += 1;
    }

    if (i == 0) {
        while (i < strlen(s)) {
            if (s[i] != 'b') {
                break;
            }
            i += 1;
        }
        assert(i == strlen(s));
    }
}
