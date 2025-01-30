#include <assert.h>
#include <string.h>

void f220(const char* s) {
    int i = 0;
    while (i < strlen(s) - 1) {
        assert(s[i] == 'a');
        i++;
    }
    assert(s[i] == 'b');
}
