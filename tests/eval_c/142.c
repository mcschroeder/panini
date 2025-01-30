#include <string.h>
#include <stdlib.h>

void f142(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] == 'a') {
            return;
        }
        i++;
    }
    exit(1);
}
