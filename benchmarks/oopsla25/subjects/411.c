#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f411(const char *s) {
    int i = 0;
    while (i < strlen(s)) {
        if (s[i] == 'a') {
            i += 1;
        } else {
            int j = 0;
            while (j < strlen(s) - i) {
                if (s[i + j] == 'b') {
                    j += 1;
                }
                break;
            }
            assert(j > 0);
            i += j;
        }
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
  f411(my_string);
  return 0;
}
