def f194(s: str):
  i = len(s)-1
  assert s[i] == "a"
  while i > 0:
    i = i-1
    assert s[i] != "a"
