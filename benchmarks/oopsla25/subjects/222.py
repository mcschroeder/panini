#!/usr/bin/env python3

import sys

def f222(s: str):
  assert s.index("b") == len(s)-1
  i = 0
  while i < len(s)-1:
    assert s[i] == "a"
    i += 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f222(my_string)
