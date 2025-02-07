#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>

bool f372(const char *s) {
    if (strcmp(s, "a") == 0) {
        return true;
    } else if (strcmp(s, "b") == 0) {
        return false;
    } else {
        fprintf(stderr, "Exception\n");
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
  bool b = f372(my_string);
  printf("%s\n", b ? "True" : "False")
  return 0;
}
