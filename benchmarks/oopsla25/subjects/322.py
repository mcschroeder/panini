#!/usr/bin/env python3

import sys

def f322(s: str):
  a,x,b = s
  assert a == "a"
  assert b == "b"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f322(my_string)
