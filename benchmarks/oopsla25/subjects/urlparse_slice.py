
def urlparse(url: str):
  p = url
  q = url.find("://")
  if q < 0:
    q = url.find("::")
    if q >= 0:
      scheme = "tftp"
      host = p[:q]
      path = p[q+2:]
      type = "URL_OLD_TFTP"
      return
    else:
      path = p
      type = "URL_PREFIX"
      return
  
  type = "URL_NORMAL"
  
  scheme = p[:q]
  p = p[q+3:]

  q = p.find("/")
  if q >= 0:
    path = p[q+1:]
    p = p[:q]
    q = p.find('#')
    if q >= 0:
      path = path[:q]
  else:
    path = ""
  
  r = p.find("@")
  if r >= 0:
    user = p[:r]
    p = p[r+1:]
    s = p.find(":")
    if s >= 0:
      passwd = p[s+1:]
      p = p[:s]
  
  host = p
  
  r = p.find(":")
  if r >= 0:
    host = p[:r]
    port = int(p[r+1:])
  