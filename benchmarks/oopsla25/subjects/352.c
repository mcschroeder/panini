#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void f352(const char *s) {
    char a = '\0', b = '\0', x = '\0';
    
    if (strlen(s) == 2) {
        a = s[0];
        b = s[1];
    }
    
    if (strlen(s) == 3) {
        a = s[0];
        x = s[1];
        b = s[2];
    }
    
    assert(a == 'a');
    assert(b == 'b');
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
  f352(my_string);
  return 0;
}
