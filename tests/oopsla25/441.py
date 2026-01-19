#!/usr/bin/env python3

import sys

def f441(s: str):
  if s[0] == "a":
    assert s[1] == "b"
    assert len(s) <= 2
  else:
    assert s[0] == "c"
    assert len(s) == 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f441(my_string)
