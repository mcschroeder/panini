def f230(s: str):
  assert s[0] == "a"
  i = 1
  while i < len(s):
    assert s[i] == "b"
    i += 1
