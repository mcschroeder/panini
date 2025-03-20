#!/usr/bin/env python3

import sys

def f152(s: str):
  assert len(s) == 1
  assert s.find("a") == -1

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f152(my_string)
