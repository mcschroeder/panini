#include <string.h>

const char* f053(const char *s) {
    if (strlen(s) == 1) {
        return "";
    } else {
        return &s[0];
    }
}
