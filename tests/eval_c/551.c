#include <assert.h>
#include <string.h>

void f551(const char *s) {
    int i = 0;  
    while (i < strlen(s)) {
        assert(s[i] != 'b' || strlen(s) > 1);
        i++;
    }
}