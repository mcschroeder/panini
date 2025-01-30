#include <assert.h>
#include <string.h>

void f470(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == '0');
        if (i + 1 < strlen(s) && s[i + 1] == '1') {
            i += 2;
        } else {
            i += 1;
        }
    }
}
