#!/usr/bin/env python3

import sys

def f291(s: str):
  if len(s) == 0:
    return
  if s[0] == "a":
    if len(s) == 1:
      return
    elif len(s) == 2:
      assert s[1] == "b"
    else:
      raise Exception
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
  
  f291(my_string)
