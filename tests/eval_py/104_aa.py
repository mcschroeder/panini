def f104(s: str):
  i = len(s)
  while i > 0:
    assert s[i-1] == "a"
    i -= 1
  assert len(s) == 2
