#include <assert.h>
#include <string.h>

void f380(const char *s) {
    if (strcmp(s, "b") == 0) {
        return;
    } else {
        int i = 0;
        while (i < strlen(s)) {
            assert(s[i] == 'a');
            i += 1;
        }
    }
}
