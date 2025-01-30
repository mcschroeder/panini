#include <assert.h>
#include <string.h>

void f273(const char* s) {
    char c = s[strlen(s) - 1];
    if (c == 'b') {
        assert(strlen(s) == 2);
        assert(s[0] == 'a');
    } else {
        assert(c == 'a');
        assert(strlen(s) == 1);
    }
}
