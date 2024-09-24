def f183(s: str):
  assert s[len(s)-1] == "a"
  assert s[len(s)-2] != "a"
  assert len(s) == 2
