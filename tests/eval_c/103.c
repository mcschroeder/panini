#include <string.h>
#include <assert.h>

void f103(const char* s) {
    int i = 0;
    int j = 0;
    while (i < strlen(s)) {
        assert(s[i] == 'a');
        i++;
        j++;
    }
    assert(j == 2);
}
