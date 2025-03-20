#!/usr/bin/env python3

import sys

def f230(s: str):
  assert s[0] == "a"
  i = 1
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
  
  f230(my_string)
