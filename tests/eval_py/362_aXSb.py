def f362(s: str):
  i = 0  
  assert s[i] == "a"
  while i < len(s)-1:
    i += 1
  assert s[i] == "b"
