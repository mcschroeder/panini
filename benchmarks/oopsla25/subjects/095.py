#!/usr/bin/env python3

import sys

def f095(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "a"
    i += 1
  assert i == 0 or i == 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f095(my_string)
