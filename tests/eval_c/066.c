#include <stdio.h>
#include <string.h>
#include <stdlib.h>

const char* f066(const char *s) {
    if (strlen(s) > 3) {
        fprintf(stderr, "Exception\n");
        exit(1);
    } else {
        static char result[4];
        strncpy(result, s, 3);
        result[3] = '\0';
        return result;
    }
}
