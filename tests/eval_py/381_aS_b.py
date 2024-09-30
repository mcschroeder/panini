def f381(s: str):
  i = 0
  while i < len(s):
    if s[i] != "a":
      break
    i += 1
  if i < len(s):
    assert s == "b"
