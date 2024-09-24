def f441(s: str):
  if s[0] == "a":
    assert s[1] == "c"
    assert len(s) <= 2
  else:
    assert s[0] == "c"
    assert len(s) == 1
