def f272(s: str):
  bi = s.find("b")
  if bi == 1:
    assert s[0] == "a"
    assert len(s) == 2
  else:
    assert s == "a"
