#!/usr/bin/env python3

import sys

def getAddrSpec(email):
  b1 = email.index('<',0)+1
  b2 = email.index('>',b1)
  return email[b1:b2]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(getAddrSpec(my_string))
