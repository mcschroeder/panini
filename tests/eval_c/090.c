#include <string.h>
#include <assert.h>

void f090(const char* s) {
    if (strlen(s) > 0) {
        assert(strcmp(s, "a") == 0);
    }
}
