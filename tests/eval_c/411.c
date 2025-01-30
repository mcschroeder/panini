#include <assert.h>
#include <string.h>

void f411(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] == 'a') {
            i += 1;
        } else {
            int j = 0;
            while (j < strlen(s) - i) {
                if (s[i + j] == 'b') {
                    j += 1;
                }
                break;
            }
            assert(j > 0);
            i += j;
        }
    }
}
