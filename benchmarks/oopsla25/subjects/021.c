#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char f021(const char *s) {
    if (strlen(s) == 1) {
        return s[0];
    } else if (strlen(s) == 0) {
        return 'a';
    } else {
        fprintf(stderr, "Exception\n");
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
  char c = f021(my_string);
  printf("%c\n", c);
  return 0;
}
