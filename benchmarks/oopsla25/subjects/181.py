#!/usr/bin/env python3

import sys

def f181(s: str):
  x,y = s
  assert x != "a"
  assert y == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f181(my_string)
