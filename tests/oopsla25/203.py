#!/usr/bin/env python3

import sys

def f203(s: str):
  t = s[0:len(s)-1]
  i = 0
  while i < len(t):
    assert t[i] == "a"
    i += 1
  assert s[len(s)-1] != "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f203(my_string)
