#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f222(const char* s) {
    assert(strrchr(s, 'b') == s + strlen(s) - 1);
    int i = 0;
    while (i < strlen(s) - 1) {
        assert(s[i] == 'a');
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
  f222(my_string);
  return 0;
}
