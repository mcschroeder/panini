#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f263(const char* s) {
    char c = s[strlen(s) - 1];
    assert(c == 'b');
    if (strlen(s) == 2) {
        assert(s[0] == 'a');
    } else {
        assert(strlen(s) == 1);
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
  f263(my_string);
  return 0;
}
