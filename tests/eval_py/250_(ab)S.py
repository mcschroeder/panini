def f250(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "a"
    assert s[i+1] == "b"
    i += 2
