#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f241(const char* s) {
    int first_b = strchr(s, 'b') ? strchr(s, 'b') - s : strlen(s);
    int i = 0;

    while (i < first_b) {
        assert(s[i] == 'a');
        i++;
    }

    while (i < strlen(s)) {
        assert(s[i] == 'b');
        i++;
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
  f241(my_string);
  return 0;
}
