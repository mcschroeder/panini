def f154(s: str):
  if s[0] == "a":
    raise Exception
  else:
    assert len(s) == 1
