#!/usr/bin/env python3

import sys

def f380(s: str):
  if s == "b":
    return
  else:
    i = 0
    while i < len(s):
      assert s[i] == "a"
      i += 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f380(my_string)
