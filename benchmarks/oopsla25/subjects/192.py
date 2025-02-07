import sys

def f192(s: str):
  i = 0
  while i < len(s)-1:
    if s[i] == "a":
      raise Exception
    i += 1
  if s[i] != "a":
    raise Exception

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f192(my_string)
