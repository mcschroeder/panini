import sys

def f350(s: str):
  if s == "ab":
    return
  else:
    assert len(s) == 3
    assert s[0] == "a"
    assert s[2] == "b"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f350(my_string)
