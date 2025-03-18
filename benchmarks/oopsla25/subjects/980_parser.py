import sys

def parser(s):
  if s[0] == "a":
    assert len(s) == 1
  else:
    assert s[1] == "b"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  parser(my_string)
