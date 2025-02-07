import sys

def f330(s: str):
  if len(s) == 0:
    return
  else:
    assert s[0] == "a"
    assert s[2] == "b"
    assert len(s) == 3

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f330(my_string)
