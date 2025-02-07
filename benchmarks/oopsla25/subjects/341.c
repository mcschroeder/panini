#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f341(const char* s) {
    int i = 0;
    while (i < strlen(s)) {
        char a = s[i];
        char x = s[i+1];
        char b = s[i+2];
        assert(a == 'a');
        assert(b == 'b');
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
  f341(my_string);
  return 0;
}
