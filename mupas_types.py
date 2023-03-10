"""Type metadata for muPas values, variables, etc.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This module defines the system of types in muPas. This is modest in comparison
to real Pascal: only scalar, string, and numeric array values are supported,
and arrays can only be one dimensional or two dimensional. Some types defined
here aren't used in the rest of the compiler but could become useful in later
versions with more features.
"""

from typing import Sequence


class Type:
  """Base class for muPas types."""
  def __str__(self) -> str:
    return self.__class__.__name__  # A fallback that limits clutter.


class CodePointer(Type):
  """muPas pointers to code.

  Since muPas does not support user-accessible pointers or subroutine-type
  parameters (for now), this type is for return addresses.
  """


class DataPointer(Type):
  """muPas pointers to data.

  Since muPas does not support user-accessible pointers (for now), this type
  is for stack and frame pointers.
  """


class ScalarNumber(Type):
  """Base class for muPas scalar numbers."""


class Real(ScalarNumber):
  """Floating-point number.

  There is no extra information for a Real other than its identity."""


class Ordinal(ScalarNumber):
  """Base class for ordinal types.

  Ordinal types are ranged integers, essentially. This base class includes
  helper methods that code can use to implement `ord`, `pred`, and `succ`,
  which are muPas functions that can be used on all ordinal values.

  (The Tektronix BASIC architecture does not natively support integers, but
  we can pretend by doing our best to store integer values in floating-point
  numbers.)

  Implies the presence of `lower_bound` and `upper_bound` properties. Both
  bounds are inclusive.
  """
  lower_bound: int
  upper_bound: int


class Boolean(Ordinal):
  """A boolean value."""
  lower_bound = 0
  upper_bound = 1


class Char(Ordinal):
  """A char value."""
  lower_bound = 0
  upper_bound = 255


class Integer(Ordinal):
  """A signed integer value."""
  lower_bound = -230897441832960  # Tektronix BASIC limits.
  upper_bound = 230897441832960   # (Determined empirically.)


class Longint(Integer):
  """A longint is just an integer in muPas."""


class IntegerSubrange(Integer):
  """Integer subrange with inclusive upper and lower bounds."""

  def __init__(self, lower_bound: int, upper_bound: int):
    super().__init__()
    self.lower_bound = lower_bound
    self.upper_bound = upper_bound

  def __str__(self) -> str:
    return f'{self.lower_bound}..{self.upper_bound}'


class LongintSubrange(IntegerSubrange):
  """A longint subrange is just an integer subrange in muPas."""


class Enumerated(IntegerSubrange):
  """Enumerated types."""
  identifiers: tuple[str, ...]

  def __init__(self, identifiers: Sequence[str]):
    super().__init__(0, len(identifiers) - 1)
    self.identifiers = tuple(identifiers)

  def to_int(self, identifier: str) -> int:
    """Convert symbolic enumeration identifier to a number."""
    return self.identifiers.index(identifier)

  def __str__(self) -> str:
    return f"({','.join(self.identifiers)})"


class String(Type):
  """A text string."""
  length: int

  def __init__(self, length: int):
    self.length = length

  def __str__(self) -> str:
    return f'String[{self.length}]'


class Array1d(Type):
  """A 1-D array of values."""
  index_typeinfo: Ordinal
  value_typeinfo: Type

  def __init__(self, index_typeinfo: Ordinal, value_typeinfo: Type):
    super().__init__()
    self.index_typeinfo = index_typeinfo
    self.value_typeinfo = value_typeinfo

  def __str__(self) -> str:
    return f'ARRAY [{self.index_typeinfo}] OF {self.value_typeinfo}'


class Array2d(Type):
  """A 2-D array of values."""
  row_index_typeinfo: Ordinal
  col_index_typeinfo: Ordinal
  value_typeinfo: Type

  def __init__(self,
               row_index_typeinfo: Ordinal,
               col_index_typeinfo: Ordinal,
               value_typeinfo: Type):
    super().__init__()
    self.row_index_typeinfo = row_index_typeinfo
    self.col_index_typeinfo = col_index_typeinfo
    self.value_typeinfo = value_typeinfo

  def __str__(self) -> str:
    return (f'ARRAY [{self.row_index_typeinfo},{self.col_index_typeinfo}] OF '
            f'{self.value_typeinfo}')


class SubroutineParameter:
  """Container for subroutine parameters."""
  name: str
  typeinfo: Type
  reference: bool  # corresponds to VAR parameter qualifiers.

  def __init__(self, name: str, typeinfo: Type, reference: bool):
    self.name = name
    self.typeinfo = typeinfo
    self.reference = reference

  def __str__(self) -> str:
    return f"{'VAR ' if self.reference else ''}{self.typeinfo}"


class Subroutine(Type):
  """Superclass for subroutines."""
  parameters: tuple[SubroutineParameter, ...]

  def __init__(self, parameters: Sequence[SubroutineParameter]):
    super().__init__()
    self.parameters = tuple(parameters)


class Procedure(Subroutine):
  """A procedure, which only has parameters."""

  def __str__(self) -> str:
    parameters = (f" ({', '.join(str(p) for p in self.parameters)})"
                  if self.parameters else '')
    return f'PROCEDURE{parameters}'


class Function(Subroutine):
  """A function, which has a return value."""
  return_typeinfo: ScalarNumber

  def __init__(self,
               parameters: Sequence[SubroutineParameter],
               return_typeinfo: ScalarNumber):
    super().__init__(parameters)
    self.return_typeinfo = return_typeinfo

  def __str__(self) -> str:
    parameters = (f" ({', '.join(str(p) for p in self.parameters)})"
                  if self.parameters else '')
    return f'FUNCTION{parameters}: {self.return_typeinfo}'
