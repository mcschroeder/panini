import sys

def f143(s: str):
  i = len(s)
  while i > 0:
    if s[i-1] == "a":
      return
    i -= 1
  raise Exception

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f143(my_string)
