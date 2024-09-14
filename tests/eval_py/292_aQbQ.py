def f292(s: str):
  if len(s) == 0:
    return
  bi = s.find("b")
  if bi == 0:
    assert len(s) == 1
  elif bi == 1:
    assert s.index("a") == 0
    assert len(s) == 2
  else:
    assert s == "a"
