import sys

def f341(s: str):
  i = 0
  while i < len(s):
    a,x,b = s[i:i+3]
    assert a == "a"
    assert b == "b"
    i += 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f341(my_string)
