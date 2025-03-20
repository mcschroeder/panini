#!/usr/bin/env python3

import sys

def f273(s: str):
  c = s[len(s)-1]
  if c == "b":
    assert len(s) == 2
    assert s[0] == "a"
  else:
    assert c == "a"
    assert len(s) == 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f273(my_string)
