#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f560(const char *s) {
    char *bi = strchr(s, 'b');
    if (bi == s + 1) {
        assert(s[0] == 'a');
    } else {
        assert(bi == s);
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
  f560(my_string);
  return 0;
}
