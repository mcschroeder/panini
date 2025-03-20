#!/usr/bin/env python3

import sys

def f013(s: str):
  t: str = s[0:]
  assert len(t) <= 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f013(my_string)
