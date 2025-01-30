#include <assert.h>
#include <string.h>

void f163(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] != 'a');
        i++;
    }
    assert(i < 2);
}
