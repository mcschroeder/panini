#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f104(const char* s) {
    int i = strlen(s);
    while (i > 0) {
        assert(s[i - 1] == 'a');
        i--;
    }
    assert(strlen(s) == 2);
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
  f104(my_string);
  return 0;
}
