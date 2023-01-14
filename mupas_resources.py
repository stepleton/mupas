"""Abstractions for some kinds of resources that compiled muPas code might use.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.
"""


class Value:
  """Superclass for a place where a value is stored.

  The contents of this class will vary considerably depending on the target
  platform, but it should contain enough information for the code generation
  machinery to know how to refer uniquely to a value's storage resource across
  all contexts where it needs to be used.
  """


class Frame:
  """Superclass for all of a subroutine's local storage.

  In nearly all cases, this will be a stack frame (hence the name), but
  because you never know, this superclass keeps things very abstract.

  The contents of this class will vary considerably depending on the target
  platform, but it should contain enough information for the code generation
  machinery to know how to refer uniquely to a resource for a subroutine's
  local storage across all contexts where it needs to be used.
  """
