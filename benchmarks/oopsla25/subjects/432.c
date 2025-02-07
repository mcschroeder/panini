#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>

bool f432(const char *s) {
    if (s[0] == 'b') {
        fprintf(stderr, "Exception\n");
        exit(1);
    }
    
    size_t n = strlen(s);
    if (s[n - 1] == 'a' || s[n - 1] == 'b') {
        if (n > 1) {
            fprintf(stderr, "Exception\n");
            exit(1);
        }
        return true;
    } else {
        assert(n == 1);
        return false;
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
  bool b = f432(my_string);
  printf("%s\n", b ? "True" : "False");
  return 0;
}
