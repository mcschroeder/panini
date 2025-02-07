#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

const char* f066(const char *s) {
    if (strlen(s) > 3) {
        fprintf(stderr, "Exception\n");
        exit(1);
    } else {
        static char result[4];
        strncpy(result, s, 3);
        result[3] = '\0';
        return result;
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
  f066(my_string);
  return 0;
}
