#!/usr/bin/env python3

import sys

def f060(s: str):
  assert len(s) == 3

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f060(my_string)
