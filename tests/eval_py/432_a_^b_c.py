def f432(s: str):
  if s[0] == "b":
    raise Exception
  n = len(s)
  if s[n-1] == "a" or s[n-1] == "b":
    if n > 1:
      raise Exception
    return True
  else:
    assert len(s) == 1
    return False
  