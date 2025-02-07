#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f042(const char *s) {
    size_t n = strlen(s);
    if (n > 2) {
        fprintf(stderr, "Exception\n");
        exit(1);
    } else {
        char c1 = s[n - 1];
        char c2 = s[n - 2];
        return;
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
  f042(my_string);
  return 0;
}
