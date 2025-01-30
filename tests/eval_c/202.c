#include <assert.h>
#include <string.h>

void f202(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] != 'a') {
            break;
        }
        i++;
    }
    assert(i == strlen(s) - 2);
}
