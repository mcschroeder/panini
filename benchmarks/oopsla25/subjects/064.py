#!/usr/bin/env python3

import sys

def f064(s: str):
  i = 0
  while i < len(s):
    c = s[i]
    i += 1
  assert i == 3

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f064(my_string)
