#include <string.h>
#include <assert.h>

void f124(const char* s) {
    int i = 0;
    assert(s[i] == 'a');
    while (i < strlen(s)) {
        char c = s[i];
        i++;
    }
}
