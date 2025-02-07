import sys

def f123(s: str):
  i = 0
  assert len(s) > 0
  while i < len(s):
    assert i > 0 or s[i] == "a"
    i += 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f123(my_string)
