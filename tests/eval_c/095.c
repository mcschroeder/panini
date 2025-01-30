#include <string.h>
#include <assert.h>

void f095(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == 'a');
        i++;
    }
    assert(i == 0 || i == 1);
}
