#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f481(const char *s) {
    if (strlen(s) == 0) {
        return;
    }
    int i = 0;
    while (i < strlen(s) - 1) {
        if (s[i] == '1') {
            assert(s[i + 1] != '1');
        } else {
            assert(s[i] == '0');
        }
        i++;
    }
    if (strlen(s) > 1) {
        if (s[i - 1] == '1') {
            assert(s[i] == '0');
        } else {
            assert(s[i] == '0' || s[i] == '1');
        }
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
  f481(my_string);
  return 0;
}
