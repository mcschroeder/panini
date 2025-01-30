#include <assert.h>
#include <string.h>

void f451(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        char a = s[i];
        char x = s[i + 1];
        char b = s[i + 2];
        
        assert(a == 'a');
        assert(b == 'b');
        assert(x != 'a' && x != 'b');
        
        i += 1;
    }
}
