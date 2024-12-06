def f530(s: str):
  i = 0
  while i < len(s):
    if s[i] == "a":
      assert s[i+1] == "b"
      i += 2
    else:
      i += 1
