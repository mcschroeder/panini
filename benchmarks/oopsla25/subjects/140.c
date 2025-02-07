#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int f140(const char* s) {
    const char* ptr = strchr(s, 'a');
    if (ptr != NULL) {
        return ptr - s;
    }
    return -1;
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
  int i = f140(my_string);
  printf("%d\n", i);
  return 0;
}
