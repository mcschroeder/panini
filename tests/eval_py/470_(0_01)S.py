def f470(s: str):
  i = 0
  while i < len(s):
    if s[i] == "1":
      assert i > 0
      if i < len(s)-1:
        assert s[i+1] == "0"
    else:
      assert s[i] == "0"
    i += 1
