#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char* f076(const char *s) {
    size_t len = strlen(s);
    char* result = (char*)malloc(len - 1);
    if (result) {
        strncpy(result, s, len - 2);
        result[len - 2] = '\0';
    }    
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
  char *s = f076(my_string);
  printf("%s\n", s);
  return 0;
}
