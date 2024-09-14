def getAddrSpec(email):
  b = email.index('<',0)+1
  return email[b:email.index('>',b)]
