#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f223(const char* s) {
    int i = 0;
    while (i < strlen(s) - 1) {
        assert(strchr(s + i, 'a') == s + i);
        i++;
    }
    assert(strchr(s + i, 'b') == s + i);
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
  f223(my_string);
  return 0;
}
