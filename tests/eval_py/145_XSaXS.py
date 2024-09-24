def f145(s: str):
  i = 0
  f = False
  while i < len(s):
    f = f or s[i] == "a"      
    i += 1
  assert f
