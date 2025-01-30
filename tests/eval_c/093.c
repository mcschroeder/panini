#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void f093(const char* s) {
    if (strlen(s) == 0) {
        return;
    } else if (strcmp(s, "a") == 0) {
        return;
    } else {
        exit(1);
    }
}
