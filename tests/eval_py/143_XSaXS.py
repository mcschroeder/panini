def f143(s: str):
  i = len(s)
  while i > 0:
    if s[i-1] == "a":
      return
    i -= 1
  raise Exception
