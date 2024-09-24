def f164(s: str):
  if s == "":
    return
  elif s[0] == "a":
    raise Exception
  else:
    assert len(s) == 1
