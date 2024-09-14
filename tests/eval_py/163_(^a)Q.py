def f163(s: str):
  i = 0
  while i < len(s):
    assert s[i] != "a"
    i += 1
  assert i < 2
