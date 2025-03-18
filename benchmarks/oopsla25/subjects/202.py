import sys

def f202(s: str):
  i = 0
  while i < len(s):
    if s[i] != "a":
      break
    i += 1
  assert i == len(s)-1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f202(my_string)
