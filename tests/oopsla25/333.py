#!/usr/bin/env python3

import sys

def f333(s: str):    
  if len(s) > 0:
    a,x = s[0:2]
    assert a == "a"
  if len(s) > 1:
    y,b = s[1:3]  
    assert b == "b"
  if len(s) > 3:
    raise Exception

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f333(my_string)
