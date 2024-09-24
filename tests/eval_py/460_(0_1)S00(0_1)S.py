def f460(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "0" or s[i] == "1"
    i += 1
  i = 0
  while i < len(s)-1:
    if s[i] == "0" and s[i+1] == "0":
      return
    i += 1
  if len(s) >= 2:
    if s[-1] == "0" and s[-2] == "0":
      return
  raise Exception
