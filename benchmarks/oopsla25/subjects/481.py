#!/usr/bin/env python3

import sys

def f481(s: str):
  if s == "":
    return
  i = 0
  while i < len(s)-1:
    if s[i] == "1":
      assert s[i+1] != "1"
    else:
      assert s[i] == "0"
    i += 1
  if len(s) > 1:
    if s[i-1] == "1":
      assert s[i] == "0"
    else:
      assert s[i] == "0" or s[i] == "1"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f481(my_string)
