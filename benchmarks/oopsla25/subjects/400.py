#!/usr/bin/env python3

import sys

def f400(s: str):
  i = 0
  while i < len(s):
    if s[i] != "a":
      break
    i += 1
  if i == 0:
    while i < len(s):
      if s[i] != "b":
        break
      i += 1
    assert i == len(s)

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f400(my_string)
