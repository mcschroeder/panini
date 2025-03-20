#!/usr/bin/env python3

import sys

def f182(s: str):
  if s[0] == "a":
    raise Exception
  elif s[1] == "a":
    assert len(s) == 2
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
  
  f182(my_string)
