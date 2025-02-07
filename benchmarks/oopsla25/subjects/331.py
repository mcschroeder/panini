import sys

def f331(s: str):
  if s == "acb" or s == "":
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
  
  f331(my_string)
