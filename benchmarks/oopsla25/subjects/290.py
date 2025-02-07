import sys

def f290(s: str):
  assert s == "" or s == "a" or s == "b" or s == "ab"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f290(my_string)
