def f410(s: str):
  i = 0
  while i < len(s):
    assert s[i] == "a" or s[i] == "b"
    i += 1
