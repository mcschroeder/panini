#include <assert.h>
#include <string.h>

void f251(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        char a = s[i];
        char b = s[i + 1];
        assert(a == 'a');
        assert(b == 'b');
        i += 2;
    }
}
