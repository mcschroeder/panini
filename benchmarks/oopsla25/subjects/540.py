import sys

def f540(s: str):
  i = 0
  while i < len(s):
    if s[i] == "a":
      assert s[i+1] == "b"
      assert s[i+2] == "b"
      i += 3
    else:
      i += 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f540(my_string)
