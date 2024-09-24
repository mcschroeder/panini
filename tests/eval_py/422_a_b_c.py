def f422(s: str):
  c = s[0]
  if c != "a":
    if c != "b":
      if c != "c":
        raise Exception
  assert len(s) == 1
  