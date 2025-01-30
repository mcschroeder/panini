#include <string.h>
#include <assert.h>

void f111(const char* s) {
    int i = strlen(s);
    while (i > 0) {
        assert(s[i - 1] == 'a');
        i--;
    }
}
