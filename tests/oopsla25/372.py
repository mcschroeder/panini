#!/usr/bin/env python3

import sys

def f372(s: str):
  if s == "a":
    return True
  elif s == "b":
    return False
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
  
  print(f372(my_string))
