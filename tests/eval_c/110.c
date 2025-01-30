#include <string.h>
#include <assert.h>

void f110(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == 'a');
        i++;
    }
}
