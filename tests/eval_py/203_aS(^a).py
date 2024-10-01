def f203(s: str):
  t = s[0:len(s)-1]
  i = 0
  while i < len(t):
    assert t[i] == "a"
    i += 1
  assert s[len(s)-1] != "a"
