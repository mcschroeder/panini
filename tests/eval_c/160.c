#include <assert.h>
#include <string.h>

void f160(const char* s) {
    if (strlen(s) == 1) {
        assert(s[0] != 'a');
    } else {
        assert(strlen(s) == 0);
    }
}
