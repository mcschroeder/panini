#include <assert.h>
#include <string.h>

const char* f067(const char *s) {
    assert(strlen(s) <= 3);
    
    static char result[4];
    strncpy(result, s, 3);
    result[3] = '\0';
    
    return result;
}
