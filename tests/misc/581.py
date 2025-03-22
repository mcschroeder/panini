#!/usr/bin/env python3

import sys

def f581(url: str):
  d = url.find('/')
  if d >= 0:
    a = url[:d]
    assert a == "a"
  else:
    a = url
    assert a == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f581(my_string)
