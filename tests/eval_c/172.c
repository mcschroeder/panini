#include <stdlib.h>
#include <string.h>

void f172(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] == 'a') {
            exit(1);
        }
        i++;
    }
}
