
int main(int argc, char *argv[]) {
  char my_string[10240] = {0};
  if (argc == 1) {
    int chars = read(fileno(stdin), my_string, 10240);
  } else {
    int fd = open(argv[1], O_RDONLY);
    int chars = read(fd, my_string, 10240);    
    close(fd);
  }
  printf("val: <%s>\n", my_string);
  f__SUBJECT__(my_string);
  return 0;
}
