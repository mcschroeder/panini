def f231(s: str):
  i = len(s)
  while i > 0:
    i -= 1    
    if s[i] != "b":
      break
  assert s[i] == "a"
