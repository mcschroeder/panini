def f331(s: str):
  if s == "acb" or s == "":
    return
  else:
    assert len(s) == 3
    assert s[0] == "a"
    assert s[2] == "b"
