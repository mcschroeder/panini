#include <string.h>
#include <stdlib.h>

char* f076(const char *s) {
    size_t len = strlen(s);
    char* result = (char*)malloc(len - 1);
    if (result) {
        strncpy(result, s, len - 2);
        result[len - 2] = '\0';
    }    
    return result;
}
