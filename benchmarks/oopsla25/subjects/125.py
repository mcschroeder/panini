import sys

def f125(s: str):
  if s == "":
    raise Exception
  else:
    assert s[0] == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f125(my_string)
