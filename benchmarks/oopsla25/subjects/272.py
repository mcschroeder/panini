import sys

def f272(s: str):
  bi = s.find("b")
  if bi == 1:
    assert s[0] == "a"
    assert len(s) == 2
  else:
    assert s == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f272(my_string)
