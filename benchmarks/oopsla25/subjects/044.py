#!/usr/bin/env python3

import sys

def f044(s: str):
  n = len(s) - 2
  c1 = s[n]
  c2 = s[n+1]
  assert n == 0

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f044(my_string)
