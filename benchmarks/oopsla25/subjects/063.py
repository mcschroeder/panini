import sys

def f063(s: str):
  i = 0
  while i < 3:
    c = s[i]
    i += 1
  assert len(s) == i

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f063(my_string)
