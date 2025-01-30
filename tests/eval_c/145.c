#include <string.h>
#include <assert.h>

void f145(const char* s) {
    int i = 0;
    int f = 0;  // False equivalent in C
    while (i < strlen(s)) {
        f = f || (s[i] == 'a');
        i++;
    }
    assert(f);
}
