#include <assert.h>
#include <string.h>

void f214(const char* s) {
    char a = s[0];
    char b = s[1];
    char t[3] = {a, b, '\0'};
    assert(strcmp(t, "ab") == 0);
}
