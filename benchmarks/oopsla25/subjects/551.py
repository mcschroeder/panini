import sys

def f551(s: str):
  i = 0  
  while i < len(s):
    assert s[i] != "b" or len(s) > 1
    i += 1

# note: regex in .out file is equivalent to ([^b]|...*)?
if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f551(my_string)
