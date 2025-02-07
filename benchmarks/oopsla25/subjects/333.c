#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f333(const char* s) {
    if (strlen(s) > 0) {
        char a = s[0];
        char x = s[1];
        assert(a == 'a');
    }
    if (strlen(s) > 1) {
        char y = s[1];
        char b = s[2];
        assert(b == 'b');
    }
    if (strlen(s) > 3) {
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
  f333(my_string);
  return 0;
}
