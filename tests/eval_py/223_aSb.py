def f223(s: str):
  i = 0
  while i < len(s)-1:
    assert s.index("a", i) == i
    i += 1
  assert s.index("b",i) == i
