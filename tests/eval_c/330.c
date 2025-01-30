#include <string.h>
#include <assert.h>

void f330(const char* s) {
    if (strlen(s) == 0) {
        return;
    } else {
        assert(s[0] == 'a');
        assert(s[2] == 'b');
        assert(strlen(s) == 3);
    }
}
