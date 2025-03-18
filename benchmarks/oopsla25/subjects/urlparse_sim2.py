import sys

# uses_params = ['', 'ftp', 'hdl', 'prospero', 'http', 'imap',
#                'https', 'shttp', 'rtsp', 'rtspu', 'sip', 'sips',
#                'mms', 'sftp', 'tel']

# # Characters valid in scheme names
# scheme_chars = ('abcdefghijklmnopqrstuvwxyz'
#                 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
#                 '0123456789'
#                 '+-.')


# def _splitnetloc(url: str, start=0):
#     delim = len(url)   # position of end of domain part of url, default is end
#     for c in '/?#':    # look for delimiters; the order is NOT important
#         wdelim = url.find(c, start)        # find first of this delim
#         if wdelim >= 0:                    # if found
#             delim = min(delim, wdelim)     # use earliest delim position
#     return url[start:delim], url[delim:]   # return (domain, rest)

def urlparse(url: str):
    # scheme, netloc, url, query, fragment = urlsplit(url)
    #--------------------------------------------
    scheme = ''
    netloc = ''
    query = ''
    fragment = ''
    i = url.find(':')
    if i > 0:
        # if url[:i] == 'http': # optimize the common case
        #     scheme = url[:i].lower()
        #     url = url[i+1:]
        #     if url[:2] == '//':
        #         # netloc, url = _splitnetloc(url, 2)
        #         #----------------------------------------
        #         delim = len(url)
        #         wdelim = url.find('/', 2)
        #         if wdelim >= 0 and wdelim < delim:
        #           delim = wdelim
        #         # for c in '/?#':
        #         #     wdelim = url.find(c, 2)
        #         #     if wdelim >= 0:
        #         #         delim = min(delim, wdelim)
        #         netloc = url[2:delim]
        #         url = url[delim:]
        #         #----------------------------------------
        #         if (('[' in netloc and not ']' in netloc) or
        #                 (']' in netloc and not '[' in netloc)):
        #             raise ValueError("Invalid IPv6 URL")
        #     if '#' in url:
        #         url, fragment = url.split('#', 1)
        #     if '?' in url:
        #         url, query = url.split('?', 1)
        #     return #scheme, netloc, url, query, fragment        
        #----------------------------------------
        # valid_scheme = True
        # for c in url[:i]:
        #     if not c in scheme_chars:
        #         valid_scheme = False
        #         break
        #----------------------------------------
        scheme_ = url[:i]
        valid_scheme = all(c in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-.' for c in scheme_)
        #----------------------------------------        
        if valid_scheme:
            # make sure "url" is not actually a port number (in which case
            # "scheme" is really part of the path)
            rest = url[i+1:]
            if not rest or any(not c in '0123456789' for c in rest):
                # not a port number
                scheme, url = url[:i].lower(), rest


    if url[:2] == '//':
        #----------------------------------------
        # netloc, url = _splitnetloc(url, 2)
        #----------------------------------------
        wdelim = url.find('#', 2) # "/?#"
        if wdelim >= 0:
            netloc = url[2:wdelim]
            url = url[wdelim:]
        else:
            netloc = url[2:]
            url = ''
        #----------------------------------------
        if (('[' in netloc and not ']' in netloc) or
                (']' in netloc and not '[' in netloc)):
            raise ValueError("Invalid IPv6 URL")
    # if '#' in url:
    #     url, fragment = url.split('#', 1)
    # if '?' in url:
    #     url, query = url.split('?', 1)
    #--------------------------------------------

    # if scheme in uses_params and ';' in url:
    #     #----------------------------------------
    #     # url, params = _splitparams(url)
    #     #----------------------------------------
    #     if '/'  in url:
    #         i = url.find(';', url.rfind('/'))
    #         if i < 0:
    #             params = ''
    #     else:
    #         i = url.find(';')
    #     params = url[i+1:]
    #     url = url[:i]
    #     #----------------------------------------
    # else:
    #     params = ''

    return #scheme, netloc, url, params, query, fragment


if __name__ == '__main__':
  my_string = ""
  if len(sys.argv) == 1:
    my_string = sys.stdin.read()
  else:
    fd = open(sys.argv[1], 'r')
    my_string = fd.read()
    fd.close()

  urlparse(my_string)  
