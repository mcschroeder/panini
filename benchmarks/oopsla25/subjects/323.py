import sys

def f323(s: str):
  a,x = s[0:2]
  y,b = s[1:]
  assert a == "a"
  assert b == "b"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f323(my_string)
