#include <assert.h>

void f980(char *s) {
  if (s[0] == 'a') {
    assert(strlen(s) == 1);
  } else {
    assert(s[1] == 'b');
  }
}