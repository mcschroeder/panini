#include <assert.h>
#include <string.h>

void f240(const char* s) {
    int i = 0;

    while (i < strlen(s)) {
        if (s[i] != 'a') {
            break;
        }
        i++;
    }

    while (i < strlen(s)) {
        if (s[i] != 'b') {
            break;
        }
        i++;
    }

    assert(i == strlen(s));
}
