#include <assert.h>
#include <string.h>

void f351(const char *s) {
    assert(s[0] == 'a');
    if (s[1] == 'b' && strlen(s) == 2) {
        return;
    } else {
        assert(s[2] == 'b');
        assert(strlen(s) == 3);
    }
}
