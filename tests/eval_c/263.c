#include <assert.h>
#include <string.h>

void f263(const char* s) {
    char c = s[strlen(s) - 1];
    assert(c == 'b');
    if (strlen(s) == 2) {
        assert(s[0] == 'a');
    } else {
        assert(strlen(s) == 1);
    }
}
