def f114(s: str):
  i = 0
  t = ""
  while i < len(s):
    t += s[i]
    i += 1
  assert i == len(s)
  i = 0
  while i < len(t):
    assert t[i] == "a"
