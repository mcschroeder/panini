import sys

def f142(s: str):
  i = 0
  while i < len(s):
    if s[i] == "a":
      return
    i += 1
  raise Exception

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f142(my_string)
