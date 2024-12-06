def f540(s: str):
  i = 0
  while i < len(s):
    if s[i] == "a":
      assert s[i+1] == "b"
      assert s[i+2] == "b"
      i += 3
    else:
      i += 1
