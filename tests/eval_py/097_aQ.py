def f097(s: str):
  i = 0
  for c in s:
    assert c == "a"
    i += 1
  assert i == 0 or i == 1
  