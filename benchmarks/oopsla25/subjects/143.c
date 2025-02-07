#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f143(const char* s) {
    int i = strlen(s);
    while (i > 0) {
        if (s[i - 1] == 'a') {
            return;
        }
        i--;
    }
    exit(1);
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
  f143(my_string);
  return 0;
}
