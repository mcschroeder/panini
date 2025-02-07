#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f351(const char *s) {
    assert(s[0] == 'a');
    if (s[1] == 'b' && strlen(s) == 2) {
        return;
    } else {
        assert(s[2] == 'b');
        assert(strlen(s) == 3);
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
  f351(my_string);
  return 0;
}
