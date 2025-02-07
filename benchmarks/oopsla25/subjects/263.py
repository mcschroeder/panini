import sys

def f263(s: str):
  c = s[len(s)-1]
  assert c == "b"
  if len(s) == 2:
    assert s[0] == "a"
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
  
  f263(my_string)
