def f273(s: str):
  c = s[-1]
  if c == "b":
    assert len(s) == 2
    assert s[0] == "a"
  else:
    assert c == "a"
    assert len(s) == 1
