#!/usr/bin/env python3

import sys

def f262(s: str):
  bi = s.index("b")
  if bi == 1:
    assert s[0] == "a"
    assert len(s) == 2
  else:
    assert bi == 0
    assert len(s) == 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f262(my_string)
