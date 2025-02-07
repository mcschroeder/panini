import sys

def f214(s: str):
  a, b = s
  t = a + b
  assert t == "ab"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f214(my_string)
