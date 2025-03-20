#!/usr/bin/env python3

import sys

def f311(s: str):
  assert s[len(s)-2:len(s)] == "ab"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f311(my_string)
