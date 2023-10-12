import re

def from_string(string):
  m = re.search(r"(.*) <(.*?)>", string)
  if m:
    name, email = m.groups()
    return (name, email)
  else:
    m = re.search(r"<(.*)>", string)
    if m:
      return (m.group(1), None)
    return (string, None)
