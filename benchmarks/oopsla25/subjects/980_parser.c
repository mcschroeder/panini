#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f980_parser(char *s) {
  if (s[0] == 'a') {
    assert(strlen(s) == 1);
  } else {
    assert(s[1] == 'b');
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
  f980_parser(my_string);
  return 0;
}
