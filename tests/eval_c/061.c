#include <assert.h>
#include <string.h>

char* f061(const char *s) {
    assert(strlen(s) <= 3);
    
    static char result[4];
    result[0] = s[0];
    result[1] = s[1];
    result[2] = s[2];
    result[3] = '\0';

    return result;
}
