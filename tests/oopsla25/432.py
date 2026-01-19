#!/usr/bin/env python3

import sys

def f432(s: str):
  if s[0] == "b":
    raise Exception
  n = len(s)
  if s[n-1] == "a" or s[n-1] == "b":
    if n > 1:
      raise Exception
    return True
  else:
    assert len(s) == 1
    return False
  
if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f432(my_string))
