#!/usr/bin/env python3

import sys

def f121(s: str):
  assert s[0:1] == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f121(my_string)
