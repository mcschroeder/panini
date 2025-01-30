#include <assert.h>
#include <string.h>

void f194(const char* s) {
    int i = strlen(s) - 1;
    assert(s[i] == 'a');
    while (i > 0) {
        i--;
        assert(s[i] != 'a');
    }
}
