#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f511(const char *s) {
    if (strcmp(s, "abc") == 0) {
        return;
    } else if (strcmp(s, "ab") == 0) {
        return;
    } else if (strcmp(s, "a") == 0) {
        return;
    } else if (strcmp(s, "ac") == 0) {
        return;
    } else if (strcmp(s, "bc") == 0) {
        return;
    } else if (strcmp(s, "b") == 0) {
        return;
    } else if (strcmp(s, "c") == 0) {
        return;
    } else {
        assert(strcmp(s, "") == 0);
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
  f511(my_string);
  return 0;
}
