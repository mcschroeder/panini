def f351(s: str):
  assert s[0] == "a"
  if s[1] == "b":
    assert len(s) == 2
  else:
    assert s[2] == "b"
    assert len(s) == 3
