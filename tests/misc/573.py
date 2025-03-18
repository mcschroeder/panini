#!/usr/bin/env python3

import sys

def f573(url: str):
  # netloc, url = _splitnetloc(url, 2)
  #----------------------------------------
  delim = re.search("[?/#]", url)

  min(url.find(c) for c in "/?#" if c in url)

  delim = len(url)
  wdelim1 = url.find('x', 2)
  wdelim2 = url.find('y', 2)
  wdelim3 = url.find('z', 2)
  delim = min(wdelim1,wdelim2,wdelim3,delim)

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
  
  f573(my_string)
