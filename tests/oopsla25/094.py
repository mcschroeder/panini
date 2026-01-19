#!/usr/bin/env python3

import sys

def f094(s: str):
  if len(s) > 0:    
    t = s[0:1]
    assert t == "a"
    assert len(s) == 1
    return t
  else:
    return s
if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f094(my_string)
