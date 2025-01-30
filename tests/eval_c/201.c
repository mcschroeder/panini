#include <assert.h>
#include <string.h>

void f201(const char* s) {
    assert(s[strlen(s) - 1] != 'a');
    int i = 0;
    while (i < strlen(s) - 1) {
        assert(s[i] == 'a');
        i++;
    }
}
