#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f261(const char* s) {
    if (strlen(s) == 1) {
        assert(s[0] == 'b');
    } else if (strlen(s) == 2) {
        assert(s[0] == 'a');
        assert(s[1] == 'b');
    } else {
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
  f261(my_string);
  return 0;
}
