#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char* f014(const char *s) {
    if (strlen(s) != 1) {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
    char *result = malloc(2);
    strncpy(result, s, 1);
    result[1] = '\0';
    return result;
}
