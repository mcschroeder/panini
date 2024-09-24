def f263(s: str):
  c = s[-1]
  assert c == "b"
  if len(s) == 2:
    assert s[0] == "a"
  else:
    assert len(s) == 1

