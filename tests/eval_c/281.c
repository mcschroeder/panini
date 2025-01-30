#include <assert.h>
#include <string.h>

void f281(const char* s) {
    if (strlen(s) > 0) {
        assert(strcmp(s, "ab") == 0);
    }
}
