def f182(s: str):
  if s[0] == "a":
    raise Exception
  elif s[1] == "a":
    assert len(s) == 2
  else:
    raise Exception
