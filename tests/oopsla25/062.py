#!/usr/bin/env python3

import sys

def f062(s: str):
  i = 0
  c0 = s[i]
  c1 = s[i+1]
  c2 = s[i+2]
  assert len(s) == i + 3

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f062(my_string)
