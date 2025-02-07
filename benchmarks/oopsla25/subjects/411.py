import sys

def f411(s: str):
  i = 0
  while i < len(s):
    if s[i] == "a":
      i += 1
    else:
      j = 0
      while j < len(s)-i:
        if s[i+j] == "b":
          j += 1
        break
      assert j > 0
      i += j

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f411(my_string)
