def f103(s: str):
  i = 0
  j = 0
  while i < len(s):
    assert s[i] == "a"
    i += 1
    j += 1
  assert j == 2
