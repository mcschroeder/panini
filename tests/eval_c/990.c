#include <assert.h>

void f990(const char *s) {
    int i = 0;
    while (i < strlen(s) - 1) {
        assert(s[i] == '0');
        i++;
    }
    assert(s[i] == '1');
}