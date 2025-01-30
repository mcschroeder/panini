#include <assert.h>
#include <string.h>

void f352(const char *s) {
    char a = '\0', b = '\0', x = '\0';
    
    if (strlen(s) == 2) {
        a = s[0];
        b = s[1];
    }
    
    if (strlen(s) == 3) {
        a = s[0];
        x = s[1];
        b = s[2];
    }
    
    assert(a == 'a');
    assert(b == 'b');
}
