def f241(s: str):
  first_b = s.find("b")
  if first_b < 0:
    first_b = len(s)
  i = 0
  while i < first_b:
    assert s[i] == "a"
    i += 1
  while i < len(s):
    assert s[i] == "b"
    i += 1
