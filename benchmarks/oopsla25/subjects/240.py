import sys

def f240(s: str):
  i = 0
  while i < len(s):
    if s[i] != "a":
      break
    i += 1
  while i < len(s):
    if s[i] != "b":
      break
    i += 1
  assert i == len(s)

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f240(my_string)
