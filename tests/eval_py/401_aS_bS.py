def f401(s: str):
  i = 0
  ca = 0
  cb = 0
  while i < len(s):
    if s[i] == "a":
      ca += 1
    if s[i] == "b":
      cb += 1
    i += 1
  assert ca == len(s) or cb == len(s)
