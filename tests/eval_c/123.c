#include <string.h>
#include <assert.h>

void f123(const char* s) {
    int i = 0;
    assert(strlen(s) > 0);
    while (i < strlen(s)) {
        assert(i > 0 || s[i] == 'a');
        i++;
    }
}
