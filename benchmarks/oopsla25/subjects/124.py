#!/usr/bin/env python3

import sys

def f124(s: str):
  i = 0
  assert s[i] == "a"
  while i < len(s):
    c = s[i]
    i += 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f124(my_string)
