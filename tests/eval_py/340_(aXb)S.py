def f340(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "a"
    assert s[i+2] == "b"
    i += 3
