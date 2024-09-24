def f172(s: str):
  i = 0
  while i < len(s):
    if s[i] == "a":
      raise Exception
    i += 1
