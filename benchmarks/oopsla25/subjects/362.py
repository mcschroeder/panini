import sys

def f362(s: str):
  i = 0  
  assert s[i] == "a"
  while i < len(s)-1:
    i += 1
  assert s[i] == "b"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f362(my_string)
