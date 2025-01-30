#include <assert.h>
#include <string.h>

void f221(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] != 'a') {
            break;
        }
        i++;
    }
    assert(s[i] == 'b');
}
