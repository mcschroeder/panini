#include <string.h>
#include <stdlib.h>

void f125(const char* s) {
    if (strlen(s) == 0) {
        exit(1);
    } else {
        assert(s[0] == 'a');
    }
}
