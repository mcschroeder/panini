import sys

def f521(s: str):
  assert s[0] == "a"
  assert s[1] == s[0]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f521(my_string)
