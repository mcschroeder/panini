#!/usr/bin/env python3

import sys

def f391(s: str):
  if len(s) == 0:
    return
  else:
    if s[0] == "a":
      assert len(s) == 1
    else:
      assert s[0] == "b"
      assert len(s) == 1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f391(my_string)
