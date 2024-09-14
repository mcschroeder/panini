def f096(s: str):
  for c in s:
    assert c == "a"
  assert len(s) >= 0 and len(s) <= 1
