#include <assert.h>
#include <string.h>

void f241(const char* s) {
    int first_b = strchr(s, 'b') ? strchr(s, 'b') - s : strlen(s);
    int i = 0;

    while (i < first_b) {
        assert(s[i] == 'a');
        i++;
    }

    while (i < strlen(s)) {
        assert(s[i] == 'b');
        i++;
    }
}
