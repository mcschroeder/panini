def f111(s: str):
  i = len(s)
  while i > 0:
    assert s[i-1] == "a"
    i -= 1
