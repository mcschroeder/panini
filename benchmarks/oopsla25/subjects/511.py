import sys

def f511(s: str):
  if s == "abc":
    return
  elif s == "ab":
    return
  elif s == "a":
    return
  elif s == "ac":
    return
  elif s == "bc":
    return
  elif s == "b":
    return
  elif s == "c":
    return
  else:
    assert s == ""

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f511(my_string)
