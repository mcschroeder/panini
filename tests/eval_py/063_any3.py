def f063(s: str):
  i = 0
  while i < 3:
    c = s[i]
    i += 1
  assert len(s) == i
