#!/usr/bin/env python3

import sys

def f221(s: str):
  i = 0
  while i < len(s):
    if s[i] != "a":
      break
    i += 1
  assert s[i] == "b"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f221(my_string)
