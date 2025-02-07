#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int f442(const char *s) {
    if (s[0] == 'a') {
        if (s[1] == 'b') {
            if (strlen(s) == 2) {
                return 1;
            }
        }
    } else if (s[0] == 'c') {
        if (strlen(s) == 1) {
            return 2;
        }
    }
    fprintf(stderr, "Exception\n");
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
  int i = f442(my_string);
  printf("%d\n", i);
  return 0;
}
