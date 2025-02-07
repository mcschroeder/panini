import sys

def f430(s: str):
  assert s == "a" or (s != "b" and len(s) == 1) or s == "c"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f430(my_string)
