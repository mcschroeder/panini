#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f062(const char *s) {
    int i = 0;
    char c0 = s[i];
    char c1 = s[i + 1];
    char c2 = s[i + 2];
    assert(strlen(s) == i + 3);
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
  f062(my_string);
  return 0;
}
