def f461(s: str):
  i = 0
  while i < len(s):
    if s[i] == "0":
      if i < len(s)-1:
        if s[i+1] == "0":
          break
    else:
      assert s[i] == "1"
    i += 1
  assert i < len(s)-1
  i += 1
  assert s[i] == "0"
  while i < len(s):
    assert s[i] == "0" or s[i] == "1"
    i += 1
