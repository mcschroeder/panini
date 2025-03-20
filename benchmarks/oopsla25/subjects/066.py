#!/usr/bin/env python3

import sys

def f066(s: str):
  if len(s) > 3:
    raise Exception
  else:
    return s[0:3]

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  print(f066(my_string))
