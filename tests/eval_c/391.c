#include <assert.h>
#include <string.h>

void f391(const char *s) {
    if (strlen(s) == 0) {
        return;
    } else {
        if (s[0] == 'a') {
            assert(strlen(s) == 1);
        } else {
            assert(s[0] == 'b');
            assert(strlen(s) == 1);
        }
    }
}
