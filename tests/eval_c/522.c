#include <assert.h>
#include <string.h>

void f522(const char *s) {
    int i = 0;
    while (i < 2) {
        assert(s[i] == 'a');
        i++;
    }
}
