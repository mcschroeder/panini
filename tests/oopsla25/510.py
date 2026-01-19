#!/usr/bin/env python3

import sys

def f510(s: str):
  assert s == "abc" or s == "ab" or s == "a" or s == "ac" or s == "bc" or s == "b" or s == "c" or s == ""

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f510(my_string)
