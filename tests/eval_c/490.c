#include <assert.h>
#include <string.h>

void f490(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == '0' || s[i] == '1');
        i++;
    }
    assert(strcmp(s + strlen(s) - 3, "011") == 0);
}
