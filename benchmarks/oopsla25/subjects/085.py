#!/usr/bin/env python3

import sys

def f085(s: str):
  if not len(s) == 1:
    raise Exception
  assert s[0] == 'a'
  return s[0]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f085(my_string))
