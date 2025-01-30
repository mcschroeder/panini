#include <string.h>
#include <assert.h>
#include <stdlib.h>

void f333(const char* s) {
    if (strlen(s) > 0) {
        char a = s[0];
        char x = s[1];
        assert(a == 'a');
    }
    if (strlen(s) > 1) {
        char y = s[1];
        char b = s[2];
        assert(b == 'b');
    }
    if (strlen(s) > 3) {
        exit(1);
    }
}
