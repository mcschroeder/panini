#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int f442(const char *s) {
    if (s[0] == 'a') {
        if (s[1] == 'b') {
            if (strlen(s) == 2) {
                return 1;
            }
        }
    } else if (s[0] == 'c') {
        if (strlen(s) == 1) {
            return 2;
        }
    }
    fprintf(stderr, "Exception\n");
    exit(1);
}
