#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void f422(const char *s) {
    char c = s[0];
    if (c != 'a') {
      if (c != 'b') {
        if (c != 'c') {
          fprintf(stderr, "Exception\n");
          exit(1);
        }
      }
    }
    assert(strlen(s) == 1);
}
