def f251(s: str):
  i = 0
  while i < len(s):
    a,b = s[i:i+2]
    assert a == "a"
    assert b == "b"
    i += 2
