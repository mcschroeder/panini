#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f194(const char* s) {
    int i = strlen(s) - 1;
    assert(s[i] == 'a');
    while (i > 0) {
        i--;
        assert(s[i] != 'a');
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
  f194(my_string);
  return 0;
}
