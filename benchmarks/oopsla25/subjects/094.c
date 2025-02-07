#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

const char* f094(const char* s) {
    if (strlen(s) > 0) {
        char t[2];
        t[0] = s[0];
        t[1] = '\0';
        assert(strcmp(t, "a") == 0);
        assert(strlen(s) == 1);
        return t;
    } else {
        return s;
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
  f094(my_string);
  return 0;
}
