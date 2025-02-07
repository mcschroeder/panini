import sys

def f145(s: str):
  i = 0
  f = False
  while i < len(s):
    f = f or s[i] == "a"      
    i += 1
  assert f

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f145(my_string)
