def f222(s: str):
  assert s.index("b") == len(s)-1
  i = 0
  while i < len(s)-1:
    assert s[i] == "a"
    i += 1
