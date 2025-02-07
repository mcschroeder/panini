#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f291(const char* s) {
    if (strlen(s) == 0) {
        return;
    }

    if (s[0] == 'a') {
        if (strlen(s) == 1) {
            return;
        } else if (strlen(s) == 2) {
            assert(s[1] == 'b');
        } else {
            exit(1);
        }
    } else {
        assert(s[0] == 'b');
        assert(strlen(s) == 1);
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
  f291(my_string);
  return 0;
}
