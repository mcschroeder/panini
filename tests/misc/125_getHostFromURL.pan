import axioms

-- def getHostFromURL(url: str) -> str:
--   """Return␣the␣hostname␣part␣for␣the␣given␣‘url‘."""
--   start_hostname = url.find(’://’) + 3
--   end_hostname = url.find(’/’, start_hostname)
--   hostname = url[start_hostname:end_hostname]
--   return hostname

find : (s:𝕊) → (t:𝕊) → (o:ℤ) → {i:ℤ | t = s[o+i..|t|]}

getHostFromURL : {url:string|?} -> string
getHostFromURL = \url:string.
  let start_hostname0 = find url "://" 0 in
  let start_hostname = add start_hostname0 3 in
  let end_hostname = find url "/" start_hostname in
  let hostname = slice url start_hostname end_hostname in
  hostname
