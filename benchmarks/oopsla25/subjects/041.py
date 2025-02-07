import sys

def f041(s: str):
  assert len(s) <= 2
  return s[0] + s[1]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f041(my_string))
