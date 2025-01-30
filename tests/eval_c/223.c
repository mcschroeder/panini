#include <assert.h>
#include <string.h>

void f223(const char* s) {
    int i = 0;
    while (i < strlen(s) - 1) {
        assert(strchr(s + i, 'a') == s + i);
        i++;
    }
    assert(strchr(s + i, 'b') == s + i);
}
