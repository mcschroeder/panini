#include <stdlib.h>
#include <string.h>

void f192(const char* s) {
    int i = 0;
    while (i < strlen(s) - 1) {
        if (s[i] == 'a') {
            exit(1);
        }
        i++;
    }
    if (s[i] != 'a') {
        exit(1);
    }
}
