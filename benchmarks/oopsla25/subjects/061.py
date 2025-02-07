import sys

def f061(s: str) -> str:
  assert len(s) <= 3
  c0 = s[0]
  c1 = s[1]
  c2 = s[2]
  return c0 + c1 + c2

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f061(my_string))
