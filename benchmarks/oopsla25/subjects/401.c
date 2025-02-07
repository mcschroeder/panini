#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f401(const char *s) {
    int i = 0;
    int ca = 0;
    int cb = 0;
    while (i < strlen(s)) {
        if (s[i] == 'a') {
            ca += 1;
        }
        if (s[i] == 'b') {
            cb += 1;
        }
        i++;
    }
    assert(ca == strlen(s) || cb == strlen(s));
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
  f401(my_string);
  return 0;
}
