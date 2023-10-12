# https://github.com/psf/requests/blob/15585909c3dd3014e4083961c8a404709450151c/requests/utils.py#L713

import socket

def is_valid_cidr(string_network):
  assert string_network.count("/") == 1
  slash_index = string_network.index("/")
  mask = int(string_network[slash_index+1:])
  assert mask >= 1 and mask <= 32
  socket.inet_aton(string_network[0:slash_index-1])
