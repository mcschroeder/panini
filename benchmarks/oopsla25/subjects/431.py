#!/usr/bin/env python3

import sys

def f431(s: str):
  assert len(s) == 1
  c = s[0]
  if c != "a":
    if c != "c":
      assert c != "b"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f431(my_string)
