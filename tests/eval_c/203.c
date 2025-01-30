#include <assert.h>
#include <string.h>

void f203(const char* s) {
    int len = strlen(s);
    const char* t = strndup(s, len - 1);
    int i = 0;
    while (i < len - 1) {
        assert(t[i] == 'a');
        i++;
    }
    assert(s[len - 1] != 'a');
    free((void*)t);
}
