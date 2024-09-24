def f442(s: str):
  if s[0] == "a":
    if s[1] == "b":
      if len(s) == 2:
        return 1
  elif s[0] == "c":
    if len(s) == 1:
      return 2
  raise Exception
