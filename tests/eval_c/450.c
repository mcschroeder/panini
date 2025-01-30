#include <assert.h>
#include <string.h>

void f450(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i + 0] == 'a');
        assert(s[i + 1] != 'a' && s[i + 1] != 'b');
        assert(s[i + 2] == 'b');
        i++;
    }
}
