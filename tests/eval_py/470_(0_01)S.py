def f470(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "0"
    if i+1 < len(s) and s[i+1] == "1":
      i += 2
    else:
      i += 1
  