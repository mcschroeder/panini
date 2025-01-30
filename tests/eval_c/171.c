#include <assert.h>
#include <string.h>

void f171(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] != 'a');
        i++;
    }
}
