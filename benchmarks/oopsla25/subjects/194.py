#!/usr/bin/env python3

import sys

def f194(s: str):
  i = len(s)-1
  assert s[i] == "a"
  while i > 0:
    i = i-1
    assert s[i] != "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f194(my_string)
