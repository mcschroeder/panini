#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f323(const char* s) {
    char a = s[0];
    char x = s[1];
    char y = s[1];
    char b = s[2];
    assert(a == 'a');
    assert(b == 'b');
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
  f323(my_string);
  return 0;
}
