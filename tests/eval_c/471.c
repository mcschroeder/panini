#include <assert.h>
#include <string.h>

void f471(const char *s) {
    if (strlen(s) == 0) {
        return;
    }
    assert(s[0] == '0');
    
    int i = 0;
    while (i < strlen(s) - 1) {
        if (s[i] == '1') {
            assert(s[i + 1] != '1');
        } else {
            assert(s[i] == '0');
        }
        i++;
    }

    if (strlen(s) > 1) {
        if (s[i - 1] == '1') {
            assert(s[i] == '0');
        } else {
            assert(s[i] == '0' || s[i] == '1');
        }
    }
}
