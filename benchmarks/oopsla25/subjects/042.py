import sys

def f042(s: str):
  n = len(s)
  if n > 2:
    raise Exception
  else:
    c1 = s[n-1]
    c2 = s[n-2]
    return

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f042(my_string))
