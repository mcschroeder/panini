# https://github.com/psf/requests/blob/15585909c3dd3014e4083961c8a404709450151c/requests/utils.py#L713

import socket

def is_valid_cidr(string_network):
    """
    Very simple check of the cidr format in no_proxy variable.
    :rtype: bool
    """
    if string_network.count("/") == 1:
        try:
            mask = int(string_network.split("/")[1])
        except ValueError:
            return False

        if mask < 1 or mask > 32:
            return False

        try:
            socket.inet_aton(string_network.split("/")[0])
        except OSError:
            return False
    else:
        return False
    return True