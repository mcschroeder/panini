#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f203(const char* s) {
    int len = strlen(s);
    const char* t = strndup(s, len - 1);
    int i = 0;
    while (i < len - 1) {
        assert(t[i] == 'a');
        i++;
    }
    assert(s[len - 1] != 'a');
    free((void*)t);
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
  f203(my_string);
  return 0;
}
