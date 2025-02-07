import sys

def f261(s: str):
  if len(s) == 1:
    assert s[0] == "b"
  elif len(s) == 2:
    assert s[0] == "a"
    assert s[1] == "b"
  else:
    raise Exception

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f261(my_string)
