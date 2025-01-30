#include <stdlib.h>
#include <string.h>

void f213(const char* s) {
    char a = s[0];
    char b = s[1];
    if (a != 'a') {
        exit(1);
    }
    if (b != 'b') {
        exit(1);
    }
}
