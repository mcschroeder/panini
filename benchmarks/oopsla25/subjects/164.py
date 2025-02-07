import sys

def f164(s: str):
  if s == "":
    return
  elif s[0] == "a":
    raise Exception
  else:
    assert len(s) == 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f164(my_string)
