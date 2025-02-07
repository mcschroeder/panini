import sys

def f470(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "0"
    if i+1 < len(s) and s[i+1] == "1":
      i += 2
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
  
  f470(my_string)
