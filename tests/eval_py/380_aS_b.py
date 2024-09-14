def f380(s: str):
  if s == "b":
    return
  else:
    i = 0
    while i < len(s):
      assert s[i] == "a"
      i += 1
