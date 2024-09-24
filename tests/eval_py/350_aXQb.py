def f350(s: str):
  if s == "ab":
    return
  else:
    assert len(s) == 3
    assert s[0] == "a"
    assert s[2] == "b"
