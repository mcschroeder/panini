#include <assert.h>
#include <string.h>

void f233(const char* s) {
    int i = 0;
    assert(strchr(s + i, 'b') == s + i);
    i++;
    while (i < strlen(s)) {
        assert(strchr(s + i, 'b') == s + i);
        i++;
    }
}
