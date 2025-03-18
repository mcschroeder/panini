def test(url: str):
  i = url.find(':')
  if i > 0:
    if url[:i] == 'http':
      scheme = url[:i].lower()
      url = url[i+1:]
      if url[:2] == '//':
          delim = len(url)
          wdelim = url.find('W', 2)
          if wdelim >= 0:
             delim = wdelim
          netloc = url[:delim]
          if 'A' in netloc and not 'B' in netloc:
             raise Exception

  
  return url
  