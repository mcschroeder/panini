#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f233(const char* s) {
    int i = 0;
    assert(strchr(s + i, 'b') == s + i);
    i++;
    while (i < strlen(s)) {
        assert(strchr(s + i, 'b') == s + i);
        i++;
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
  f233(my_string);
  return 0;
}
