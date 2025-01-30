#include <assert.h>
#include <string.h>

void f392(const char *s) {
    assert(strlen(s) == 0 || (strlen(s) == 1 && (strchr(s, 'a') != NULL || strchr(s, 'b') != NULL)));
}
