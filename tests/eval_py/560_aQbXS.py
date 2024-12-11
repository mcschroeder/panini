def f560(s: str):
  bi = s.index("b")
  if bi == 1:
    assert s[0] == "a"
  else:
    assert bi == 0
