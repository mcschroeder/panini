#include <string.h>
#include <assert.h>

void f113(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] != 'a') {
            break;
        }
        i++;
    }
    assert(i == strlen(s));
}
