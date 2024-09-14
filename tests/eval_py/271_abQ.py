def f271(s: str):
  if len(s) == 1:
    assert s[0] == "a"
  elif len(s) == 2:
    assert s[0] == "a"
    assert s[1] == "b"
  else:
    raise Exception
