import sys

def f231(s: str):
  i = len(s)
  while i > 0:
    i -= 1    
    if s[i] != "b":
      break
  assert s[i] == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f231(my_string)
