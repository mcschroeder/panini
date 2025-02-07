#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f145(const char* s) {
    int i = 0;
    int f = 0;  // False equivalent in C
    while (i < strlen(s)) {
        f = f || (s[i] == 'a');
        i++;
    }
    assert(f);
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
  f145(my_string);
  return 0;
}
