#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f292(const char* s) {
    if (strlen(s) == 0) {
        return;
    }

    char* bi = strchr(s, 'b');
    if (bi == s) {
        assert(strlen(s) == 1);
    } else if (bi == s + 1) {
        assert(s[0] == 'a');
        assert(strlen(s) == 2);
    } else {
        assert(strcmp(s, "a") == 0);
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
  f292(my_string);
  return 0;
}
