def f242(s: str):
  bi = s.index("b")
  sa = s[0:bi]
  sb = s[bi:]
  i = 0
  while i < len(sa):
    assert sb[i] == "a"
    i += 1
  i = 0
  while i < len(sb):
    assert sb[i] == "b"
    i += 1
