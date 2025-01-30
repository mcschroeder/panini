#include <string.h>
#include <assert.h>

void f340(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == 'a');
        assert(s[i + 2] == 'b');
        i += 3;
    }
}
