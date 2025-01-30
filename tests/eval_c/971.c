#include <string.h>
#include <stdlib.h>

char* f971(const char *email) {
    char *b1 = strchr(email, '<') + 1;
    char *b2 = strchr(b1, '>');
    size_t length = b2 - b1;
    char *addrSpec = malloc(length + 1);
    strncpy(addrSpec, b1, length);
    addrSpec[length] = '\0';
    return addrSpec;
}
