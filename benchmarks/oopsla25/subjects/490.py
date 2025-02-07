import sys

def f490(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "0" or s[i] == "1"
    i += 1  
  assert s[-3:] == "011"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f490(my_string)
