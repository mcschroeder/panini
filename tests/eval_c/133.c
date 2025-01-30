#include <string.h>
#include <assert.h>

void f133(const char* s) {
    int i = 0;
    while (i < strlen(s) - 1) {
        i++;
    }
    assert(s[i] == 'a');
}
