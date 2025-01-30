#include <assert.h>
#include <string.h>

void f250(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == 'a');
        assert(s[i + 1] == 'b');
        i += 2;
    }
}
