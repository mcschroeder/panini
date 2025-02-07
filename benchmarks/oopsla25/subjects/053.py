import sys

def f053(s: str):
  if len(s) == 1:
    return ""
  else:
    return s[0]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f053(my_string)
