#!/usr/bin/env python3

import sys

def f320(s: str):
  assert s[0] == "a"
  assert s[2] == "b"
  assert len(s) == 3

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f320(my_string)
