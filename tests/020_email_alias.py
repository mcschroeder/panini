import re

def get_alias_from_email(email):
  if not "@" in email:
    return ""
  
  potential_alias = email[0:email.index("@")]
  if re.match(r"^[a-zA-Z0-9\\-]+$", potential_alias):
    return potential_alias
  else:
    return ""
