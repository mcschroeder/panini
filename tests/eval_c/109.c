#include <string.h>
#include <assert.h>

void f109(const char* s) {
    char a = s[0];
    char b = s[1];
    char t[3];
    t[0] = a;
    t[1] = b;
    t[2] = '\0';
    assert(strcmp(t, "aa") == 0);
}
