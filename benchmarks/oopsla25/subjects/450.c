#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f450(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        assert(s[i + 0] == 'a');
        assert(s[i + 1] != 'a' && s[i + 1] != 'b');
        assert(s[i + 2] == 'b');
        i = i+3;
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
  f450(my_string);
  return 0;
}
