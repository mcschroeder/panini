#include <assert.h>
#include <string.h>

void f086(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == 'a');
        i += 1;
    }
    assert(i == 1);
}
