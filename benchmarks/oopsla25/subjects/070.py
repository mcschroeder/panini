#!/usr/bin/env python3

import sys

def f070(s: str):
  assert len(s) >= 2

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f070(my_string)
