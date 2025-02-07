#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char* f014(const char *s) {
    if (strlen(s) != 1) {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
    char *result = malloc(2);
    strncpy(result, s, 1);
    result[1] = '\0';
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
  char *s = f014(my_string);
  printf("%s\n", s);
  return 0;
}
