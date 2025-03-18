#!/usr/bin/env python3

import sys

def f572(url: str):
  # netloc, url = _splitnetloc(url, 2)
  #----------------------------------------
  wdelim1 = url.find('x', 2)
  wdelim2 = url.find('y', 2)
  wdelim3 = url.find('z', 2)
  if wdelim1 >= 0 and wdelim1 < wdelim2 and wdelim1 < wdelim3:
      netloc = url[2:wdelim1]
      url = url[wdelim1:]
  elif wdelim2 >= 0 and wdelim2 < wdelim1 and wdelim2 < wdelim3:
      netloc = url[2:wdelim2]
      url = url[wdelim2:]
  elif wdelim3 >= 0 and wdelim3 < wdelim1 and wdelim3 < wdelim2:
      netloc = url[2:wdelim3]
      url = url[wdelim3:]
  else:
      netloc = url[2:]
      url = ''
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
  
  f572(my_string)
