import sys

def f183(s: str):
  assert s[len(s)-1] == "a"
  assert s[len(s)-2] != "a"
  assert len(s) == 2

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f183(my_string)
