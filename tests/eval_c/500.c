#include <assert.h>
#include <string.h>

void f500(const char *s) {
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
    while (i < strlen(s)) {
        if (s[i] != 'c') {
            break;
        }
        i++;
    }
    assert(i == strlen(s));
}
