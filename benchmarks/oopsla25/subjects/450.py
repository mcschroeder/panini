#!/usr/bin/env python3

import sys

def f450(s: str):
  i = 0
  while i < len(s):
    assert s[i+0] == "a"
    assert s[i+1] != "a"
    assert s[i+1] != "b"
    assert s[i+2] == "b"
    i += 3

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f450(my_string)
