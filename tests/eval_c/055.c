#include <string.h>

const char* f055(const char *s) {
    if (s[0] == 'a') {
        return "A";
    } else {
        return &s[0];
    }
}
