#include <string.h>
#include <stdlib.h>

char* f970(const char *email) {
    char *b = strchr(email, '<') + 1;
    size_t length = strchr(b, '>') - b;
    char *addrSpec = malloc(length + 1);
    strncpy(addrSpec, b, length);
    addrSpec[length] = '\0';
    return addrSpec;
}