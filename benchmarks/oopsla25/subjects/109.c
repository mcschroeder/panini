#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f109(const char* s) {
    char a = s[0];
    char b = s[1];
    char t[3];
    t[0] = a;
    t[1] = b;
    t[2] = '\0';
    assert(strcmp(t, "aa") == 0);
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
  f109(my_string);
  return 0;
}
