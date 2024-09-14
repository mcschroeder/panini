def f123(s: str):
  i = 0
  while i < len(s):
    assert s > 0 or s[i] == "a"
    i += 1
