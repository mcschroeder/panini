def lsb_check(s):
  i = 0
  while i < len(s)-1:
    assert s[i] == '0'
    i += 1
  assert s[i] == '1'
