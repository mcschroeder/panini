#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char* f072(const char *s) {
    static char r[3];
    r[0] = s[0];
    r[1] = s[1];
    r[2] = '\0';
    return r;
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
  char *r = f072(my_string);
  printf("%s\n", r);
  return 0;
}
