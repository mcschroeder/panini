int kw_ep(int argc, char* argv[]) {
  int SYMEX_SIZE = atoi(argv[1]);
  char* inp = malloc(SYMEX_SIZE);
  klee_make_symbolic(inp, SYMEX_SIZE, "input_str");
  // restrict possible inputs to printable ASCII
  for (int i = 0; i < SYMEX_SIZE; i++) {
	  klee_assume((inp[i] == '\0') | (inp[i] >= 32 & inp[i] < 127));
  }
  klee_assume(inp[SYMEX_SIZE - 1] == '\0');
  char result[10240];
  init_hex_values();
  int ret = cgidecode(inp, result);
  return ret;
}
