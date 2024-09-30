def f123(s: str):
  i = 0
  assert len(s) > 0
  while i < len(s):
    assert i > 0 or s[i] == "a"
    i += 1
