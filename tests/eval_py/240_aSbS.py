def f240(s: str):
  i = 0
  while i < len(s):
    if s[i] != "a":
      break
    i += 1
  while i < len(s):
    if s[i] != "b":
      break;
    i += 1
  assert i == len(s)
