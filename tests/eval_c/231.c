#include <assert.h>
#include <string.h>

void f231(const char* s) {
    int i = strlen(s);
    while (i > 0) {
        i--;
        if (s[i] != 'b') {
            break;
        }
    }
    assert(s[i] == 'a');
}
