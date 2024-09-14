def f252(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "a"
    i += 2
  i = 1
  while i < len(s):
    assert s[i] == "b"
    i += 2
