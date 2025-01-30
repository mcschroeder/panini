#include <assert.h>
#include <string.h>
#include <stdlib.h>

void f261(const char* s) {
    if (strlen(s) == 1) {
        assert(s[0] == 'b');
    } else if (strlen(s) == 2) {
        assert(s[0] == 'a');
        assert(s[1] == 'b');
    } else {
        exit(1);
    }
}
