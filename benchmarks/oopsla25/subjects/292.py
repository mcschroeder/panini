import sys

def f292(s: str):
  if len(s) == 0:
    return
  bi = s.find("b")
  if bi == 0:
    assert len(s) == 1
  elif bi == 1:
    assert s.index("a") == 0
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
  
  f292(my_string)
