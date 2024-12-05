def f551(s: str):
  i = 0  
  while i < len(s):
    assert s[i] != "b" or len(s) > 1
    i += 1

# note: regex in .out file is equivalent to ([^b]|...*)?