#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f065(const char *s) {
    int i = strlen(s) - 1;
    assert(i == 2);
    while (i >= 0) {
        char c = s[i];
        i -= 1;
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
  f065(my_string);
  return 0;
}
