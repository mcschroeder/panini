#include <assert.h>
#include <string.h>
#include <stdlib.h>

void f291(const char* s) {
    if (strlen(s) == 0) {
        return;
    }

    if (s[0] == 'a') {
        if (strlen(s) == 1) {
            return;
        } else if (strlen(s) == 2) {
            assert(s[1] == 'b');
        } else {
            exit(1);
        }
    } else {
        assert(s[0] == 'b');
        assert(strlen(s) == 1);
    }
}
