import sys

def f076(s: str):  
  # return s[:-2]
  return s[:len(s)-2]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f076(my_string))
