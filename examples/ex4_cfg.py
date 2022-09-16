def parser(s):
  i = 0
  j = 0
  while i < len(s) and s[i] == 'a':
    i = i + 1
    j = j + 1
  k = 0
  while i < len(s) and s[i] == 'b':
    i = i + 1
    k = k + 1
  assert i == len(s)
  assert j == k
