def f202(s: str):
  i = 0
  while i < len(s):
    if s[i] != "a":
      break
    i += 1
  assert i == len(s)-2
