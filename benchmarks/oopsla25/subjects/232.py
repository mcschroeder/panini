import sys

def f232(s: str):
  assert s.index("a") == 0
  i = 1
  while i < len(s):
    assert s[i] == "b"
    i += 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f232(my_string)
