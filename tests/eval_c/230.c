#include <assert.h>
#include <string.h>

void f230(const char* s) {
    assert(s[0] == 'a');
    int i = 1;
    while (i < strlen(s)) {
        assert(s[i] == 'b');
        i++;
    }
}
