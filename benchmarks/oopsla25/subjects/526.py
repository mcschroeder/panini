#!/usr/bin/env python3

import sys

def f526(s: str):
  a1,a2 = s[0:2]
  assert a1 == a2
  assert a1 == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f526(my_string)
