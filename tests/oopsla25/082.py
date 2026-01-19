#!/usr/bin/env python3

import sys

def f082(s: str):
  if s[0] == "a":
    assert len(s) == 1
  else:
    assert False

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f082(my_string)
