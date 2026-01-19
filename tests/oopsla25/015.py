#!/usr/bin/env python3

import sys

def f015(s: str):
  if not len(s) == 1:
    raise Exception
  return s[0]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f015(my_string))
