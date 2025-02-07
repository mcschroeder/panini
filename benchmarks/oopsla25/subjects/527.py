import sys

def f527(s: str):
  a1,a2 = s[0:2]
  assert a1 == a2
  assert s[0] == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f527(my_string)
