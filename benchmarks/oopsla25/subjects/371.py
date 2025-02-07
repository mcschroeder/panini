import sys

def f371(s: str):
  assert len(s) == 1
  if s[0] == "a":
    return
  elif s[0] == "b":
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
  
  print(f371(my_string))
