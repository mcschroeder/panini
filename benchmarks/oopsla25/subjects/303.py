#!/usr/bin/env python3

import sys

def f303(s: str):
  assert s.index("a") == 0
  assert s.index("b") == 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f303(my_string)
