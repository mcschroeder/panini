import sys

def f442(s: str):
  if s[0] == "a":
    if s[1] == "b":
      if len(s) == 2:
        return 1
  elif s[0] == "c":
    if len(s) == 1:
      return 2
  raise Exception

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f442(my_string))
