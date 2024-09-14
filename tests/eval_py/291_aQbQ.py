def f291(s: str):
  if len(s) == 0:
    return
  if s[0] == "a":
    if len(s) == 1:
      return
    elif len(s) == 2:
      assert s[1] == "b"
    else:
      raise Exception
  else:
    assert s[0] == "b"
    assert len(s) == 1
