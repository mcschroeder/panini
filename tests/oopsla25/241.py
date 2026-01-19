#!/usr/bin/env python3

import sys

def f241(s: str):
  first_b = s.find("b")
  if first_b < 0:
    first_b = len(s)
  i = 0
  while i < first_b:
    assert s[i] == "a"
    i += 1
  while i < len(s):
    assert s[i] == "b"
    i += 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f241(my_string)
