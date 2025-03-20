#!/usr/bin/env python3

import sys

def f140(s: str):
  return s.index("a")

# note: the regex [^a]*a.* is equivalent to .*a.*

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f140(my_string)
