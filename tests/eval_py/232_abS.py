def f232(s: str):
  assert s.index("a") == 0
  i = 1
  while i < len(s):
    assert s[i] == "b"
    i += 1
