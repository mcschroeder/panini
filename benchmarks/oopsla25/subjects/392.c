#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f392(const char *s) {
    assert(strlen(s) == 0 || (strlen(s) == 1 && (strchr(s, 'a') != NULL || strchr(s, 'b') != NULL)));
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
  f392(my_string);
  return 0;
}
