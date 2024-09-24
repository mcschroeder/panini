def f085(s: str):
  if not len(s) == 1:
    raise Exception
  assert s[0] == 'a'
  return s[0]
