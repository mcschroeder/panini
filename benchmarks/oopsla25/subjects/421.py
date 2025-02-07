import sys

def f421(s: str):
  assert len(s) == 1
  if s[0] == "a":
    return
  if s[0] == "b":
    return
  if s[0] == "c":
    return
  raise Exception

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f421(my_string))
