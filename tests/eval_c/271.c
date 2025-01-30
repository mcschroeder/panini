#include <assert.h>
#include <string.h>
#include <stdlib.h>

void f271(const char* s) {
    if (strlen(s) == 1) {
        assert(s[0] == 'a');
    } else if (strlen(s) == 2) {
        assert(s[0] == 'a');
        assert(s[1] == 'b');
    } else {
        exit(1);
    }
}
