def f233(s: str):
  i = 0
  assert s.index("a",i) == i
  i += 1
  while i < len(s):
    assert s.index("b", i) == i
    i += 1  
