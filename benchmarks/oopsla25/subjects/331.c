#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f331(const char* s) {
    if (strcmp(s, "acb") == 0 || strlen(s) == 0) {
        return;
    } else {
        assert(strlen(s) == 3);
        assert(s[0] == 'a');
        assert(s[2] == 'b');
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
  f331(my_string);
  return 0;
}
