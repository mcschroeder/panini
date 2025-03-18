#!/usr/bin/env python3

import sys

def f570(url: str):
  # netloc, url = _splitnetloc(url, 2)
  #----------------------------------------
  delim = len(url)
  for c in '/?#':
      wdelim = url.find(c, 2)
      if wdelim >= 0:
          delim = min(delim, wdelim)
  netloc = url[2:delim]
  url = url[delim:]
  #----------------------------------------
  assert netloc == "a"

if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()
  
  f570(my_string)
