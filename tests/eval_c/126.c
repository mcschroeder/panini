#include <string.h>
#include <assert.h>

void f126(const char* s) {
    int i = strlen(s);
    while (i > 0) {
        i--;
    }
    assert(s[i] == 'a');
}
