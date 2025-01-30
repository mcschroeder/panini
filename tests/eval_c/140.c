#include <string.h>

int f140(const char* s) {
    const char* ptr = strchr(s, 'a');
    if (ptr != NULL) {
        return ptr - s;
    }
    return -1;
}
