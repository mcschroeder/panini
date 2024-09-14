def f105(s: str):
  i = len(s)
  j = 0
  while i > 0:
    assert s[i-1] == "a"
    i -= 1
    j += 1
  assert j == 2
