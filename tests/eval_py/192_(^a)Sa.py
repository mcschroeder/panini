def f192(s: str):
  i = 0
  while i < len(s)-1:
    if s[i] == "a":
      raise Exception
    i += 1
  if s[i] != "a":
    raise Exception
