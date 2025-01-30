#include <string.h>
#include <assert.h>

void f350(const char* s) {
    if (strcmp(s, "ab") == 0) {
        return;
    } else {
        assert(strlen(s) == 3);
        assert(s[0] == 'a');
        assert(s[2] == 'b');
    }
}
