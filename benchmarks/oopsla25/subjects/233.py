#!/usr/bin/env python3

import sys

def f233(s: str):
  i = 0
  assert s.index("a",i) == i
  i += 1
  while i < len(s):
    assert s.index("b", i) == i
    i += 1  

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f233(my_string)
