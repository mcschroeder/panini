#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f490(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i] == '0' || s[i] == '1');
        i++;
    }
    assert(strcmp(s + strlen(s) - 3, "011") == 0);
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
  f490(my_string);
  return 0;
}
