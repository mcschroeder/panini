#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f182(const char* s) {
    if (s[0] == 'a') {
        exit(1);
    } else if (s[1] == 'a') {
        assert(strlen(s) == 2);
    } else {
        exit(1);
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
  f182(my_string);
  return 0;
}
