def char_range(c1, c2):
    """Generates the characters from `c1` to `c2`, inclusive."""
    for c in xrange(ord(c1), ord(c2)+1):
        yield chr(c)

lst = list(char_range('A', 'Z')) + list(char_range('a', 'z')) + list(char_range('0', '9'))
for a in lst:
  for b in lst:
    for c in lst:
      print a+b+c
