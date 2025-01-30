#include <string.h>
#include <assert.h>

void f341(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        char a = s[i];
        char x = s[i+1];
        char b = s[i+2];
        assert(a == 'a');
        assert(b == 'b');
        i++;
    }
}
