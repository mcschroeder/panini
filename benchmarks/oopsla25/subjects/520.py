#!/usr/bin/env python3

import sys

def f520(s: str):
  assert s[0] == "a"
  assert s[1] == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f520(my_string)
