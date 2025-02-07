import sys

def f392(s: str):
  assert len(s) == 0 or (len(s) == 1 and (s.find("a") >= 0 or s.find("b") >= 0))

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f392(my_string)
