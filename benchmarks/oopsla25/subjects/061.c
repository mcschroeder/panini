#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char* f061(const char *s) {
    assert(strlen(s) <= 3);
    
    static char result[4];
    result[0] = s[0];
    result[1] = s[1];
    result[2] = s[2];
    result[3] = '\0';

    return result;
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
  char *s = f061(my_string);
  printf("%s\n", s);
  return 0;
}
