#!/usr/bin/env python3

import sys

def f065(s: str):
  i = len(s)-1
  assert i == 2
  while i >= 0:
    c = s[i]
    i -= 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f065(my_string)
