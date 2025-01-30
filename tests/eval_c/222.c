#include <assert.h>
#include <string.h>

void f222(const char* s) {
    assert(strrchr(s, 'b') == s + strlen(s) - 1);
    int i = 0;
    while (i < strlen(s) - 1) {
        assert(s[i] == 'a');
        i++;
    }
}
