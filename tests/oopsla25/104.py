#!/usr/bin/env python3

import sys

def f104(s: str):
  i = len(s)
  while i > 0:
    assert s[i-1] == "a"
    i -= 1
  assert len(s) == 2

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f104(my_string)
