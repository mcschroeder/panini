def f341(s: str):
  i = 0
  while i < len(s):
    a,x,b = s[i:i+3]
    assert a == "a"
    assert b == "b"
