def f201(s: str):
  assert s[len(s)-1] != "a"
  i = 0
  while i < len(s):
    assert s[i] == "a"
    i += 1
