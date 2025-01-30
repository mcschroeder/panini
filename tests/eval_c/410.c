#include <assert.h>
#include <string.h>

void f410(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == 'a' || s[i] == 'b');
        i++;
    }
}
