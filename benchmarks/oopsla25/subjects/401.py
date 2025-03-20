#!/usr/bin/env python3

import sys

def f401(s: str):
  i = 0
  ca = 0
  cb = 0
  while i < len(s):
    if s[i] == "a":
      ca += 1
    if s[i] == "b":
      cb += 1
    i += 1
  assert ca == len(s) or cb == len(s)

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f401(my_string)
