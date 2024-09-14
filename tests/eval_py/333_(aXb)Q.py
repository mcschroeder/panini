def f333(s: str):
  if len(s) > 1:
    a,x = s[0:2]
    assert a == "a"
  if len(s) > 2:
    y,b = s[1:3]  
    assert b == "b"
  if len(s) > 3:
    raise Exception
