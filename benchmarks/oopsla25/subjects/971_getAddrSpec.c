#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char* f971_getAddrSpec(const char *email) {
    char *b1 = strchr(email, '<') + 1;
    char *b2 = strchr(b1, '>');
    size_t length = b2 - b1;
    char *addrSpec = malloc(length + 1);
    strncpy(addrSpec, b1, length);
    addrSpec[length] = '\0';
    return addrSpec;
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
  char *s = f971_getAddrSpec(my_string);
  printf("%s\n", s);
  return 0;
}
