import sys

def getAddrSpec(email):
  b = email.index('<',0)+1
  return email[b:email.index('>',b)]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(getAddrSpec(my_string))
