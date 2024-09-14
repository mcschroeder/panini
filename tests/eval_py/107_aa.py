def f107(s: str):
  if len(s) > 2:
    raise Exception
  i = 0
  for c in s:
    if c != "a":
      raise Exception
    i += 1
  if i < 1:
    raise Exception
