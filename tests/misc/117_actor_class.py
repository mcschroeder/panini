# based on https://github.com/gitpython-developers/GitPython/blob/87cc32574a2579a6a47986a103f4510e2dfe90a7/git/util.py#L759

import re

class Actor(object):

  name_only_regex = re.compile(r"<(.*)>")
  name_email_regex = re.compile(r"(.*) <(.*?)>")

  def __init__(self, name, email):
    self.name = name
    self.email = email

  @classmethod
  def from_string(cls, string):
    m = cls.name_email_regex.search(string)
    if m:
      name, email = m.groups()
      return Actor(name, email)
    else:
      m = cls.name_only_regex.search(string)
      if m:
        return Actor(m.group(1), None)
      return Actor(string, None)
