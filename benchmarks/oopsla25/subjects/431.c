#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f431(const char *s) {
    assert(strlen(s) == 1);
    char c = s[0];
    if (c != 'a') {
        if (c != 'c') {
            assert(c != 'b');
        }
    }
}

int main(int argc, char *argv[]) {
  char my_string[10240] = {0};
  if (argc == 1) {
    read(fileno(stdin), my_string, 10240);
  } else {
    int fd = open(argv[1], O_RDONLY);
    read(fd, my_string, 10240);
    close(fd);
  }
  f431(my_string);
  return 0;
}
