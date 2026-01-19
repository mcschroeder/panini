#!/usr/bin/env python3

import sys

def f021(s: str):
  if len(s) == 1:
    return s[0]
  elif len(s) == 0:
    return 'a'
  else:
    raise Exception

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f021(my_string))
