def f106(s: str):
  if len(s) != 2:
    raise Exception
  for c in s:
    if c != "a":
      raise Exception
