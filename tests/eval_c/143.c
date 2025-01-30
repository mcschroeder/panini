#include <string.h>
#include <stdlib.h>

void f143(const char* s) {
    int i = strlen(s);
    while (i > 0) {
        if (s[i - 1] == 'a') {
            return;
        }
        i--;
    }
    exit(1);
}
