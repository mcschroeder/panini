def f451(s: str):
  i = 0
  while i < len(s):
    a,x,b = s[i:i+3]
    assert a == "a"
    assert b == "b"
    assert x != "a" and x != "b"
    i += 1
