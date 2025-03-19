import sys

def f451(s: str):
  i = 0
  while i < len(s):
    a,x,b = s[i:i+3]
    assert a == "a"
    assert b == "b"
    assert x != "a" and x != "b"
    i += 3

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f451(my_string)
