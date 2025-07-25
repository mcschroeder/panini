#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f371(const char *s) {
    assert(strlen(s) == 1);
    if (s[0] == 'a') {
        return;
    } else if (s[0] == 'b') {
        return;
    }
    fprintf(stderr, "Exception\n");
    exit(1);
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
  f371(my_string);
  return 0;
}
