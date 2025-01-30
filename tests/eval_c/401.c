#include <assert.h>
#include <string.h>

void f401(const char *s) {
    int i = 0;
    int ca = 0;
    int cb = 0;
    while (i < strlen(s)) {
        if (s[i] == 'a') {
            ca += 1;
        }
        if (s[i] == 'b') {
            cb += 1;
        }
        i++;
    }
    assert(ca == strlen(s) || cb == strlen(s));
}
