"""4050-series BASIC-specific muPas type operations.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This module builds on the muPas type system: it introduces additional
*internal-only* types and type-checking facilities that facilitate code
generation. The new types tend to be relaxations or generalisations of native
mupas types that are useful for parameter type checking with "general purpose"
extensions like `WriteLn`. Meanwhile,
`check_assignment_or_parameter_compatibility` is a useful utility function for
parameter type checking.
"""

import warnings

import mupas_types

from typing import Optional


class _ForCheckingType(mupas_types.Type):
  """A Type meant exclusively for use with `check_..._compatibility` below.

  Sometimes we want to specify more generic acceptable expected types than
  what we would get by naming single types from `mupas_types`, or perhaps we
  wish to specify ranges of types, or types that are more specific. We can
  specify Type subclasses that have that special meaning to
  `check_assignment_or_parameter_compatibility`. Those that do:
    (a) should never be used to represent actual type information inside the
        compiler.
    (b) should inherit from this class.
  """


class Union(_ForCheckingType):
  """For specifying multiple acceptable expected types.

  This type class is only meant to be used 
  """
  types: tuple[mupas_types.Type]

  def __init__(self, *args: mupas_types.Type):
    types = args

  def __str__(self):
    return f'Union[{",".join(types)}]'


class String(_ForCheckingType):
  """A String type that can relax to mean a string of any length."""
  length: Optional[int]

  def __init__(self, length: Optional[int] = None):
    self.length = length

  @classmethod
  def from_string(cls, string: mupas_types.String):
    return cls(string.length)


class Array1d(_ForCheckingType):
  """An Array1d type that can relax to mean a 1-D array of any size."""
  index_typeinfo: Optional[mupas_types.Ordinal]
  value_typeinfo: mupas_types.Type

  def __init__(self,
               index_typeinfo: Optional[mupas_types.Ordinal],
               value_typeinfo: mupas_types.Type):
    super().__init__()
    self.index_typeinfo = index_typeinfo
    self.value_typeinfo = value_typeinfo

  @classmethod
  def from_array1d(cls, array: mupas_types.Array1d):
    return cls(array.index_typeinfo, array.value_typeinfo)

  def __str__(self):
    return f'{self.value_typeinfo.__class__.__name__}[{self.index_typeinfo}]'


class Array2d(_ForCheckingType):
  """An Array2d type that can relax to allow either dimension to be any size."""
  row_index_typeinfo: Optional[mupas_types.Ordinal]
  col_index_typeinfo: Optional[mupas_types.Ordinal]
  value_typeinfo: mupas_types.Type

  def __init__(self,
               row_index_typeinfo: Optional[mupas_types.Ordinal],
               col_index_typeinfo: Optional[mupas_types.Ordinal],
               value_typeinfo: mupas_types.Type):
    super().__init__()
    self.row_index_typeinfo = row_index_typeinfo
    self.col_index_typeinfo = col_index_typeinfo
    self.value_typeinfo = value_typeinfo

  @classmethod
  def from_array2d(cls, array: mupas_types.Array2d):
    return cls(array.row_index_typeinfo,
               array.col_index_typeinfo,
               array.value_typeinfo)

  def __str__(self):
    return (f'{self.value_typeinfo.__class__.__name__}'
            f'[{self.row_index_typeinfo},{self.col_index_typeinfo}]')


def check_assignment_or_parameter_compatibility(
    expected: mupas_types.Type, actual: mupas_types.Type):
  """Check type compatibility for assigment or parameter-passing.

  Raises an error if types are wholly incompatible (e.g. array size mismatch,
  a string supplied where a number is expected, etc.); issues a warning
  otherwise.

  Args:
    expected: The type specified for the variable on the left-hand side of an
        assignment or for the parameter in a subroutine parameter list.
    actual: The type of the expression being supplied as a value for the
        variable or parameter.

  Raises:
    TypeMismatch: if the types are incompatible.
  """
  def ord_range_check(ord_l: mupas_types.Ordinal, ord_r: mupas_types.Ordinal):
    range_l = ord_l.upper_bound - ord_l.lower_bound
    range_r = ord_r.upper_bound - ord_r.lower_bound
    if range_l != range_r: raise TypeMismatch(
        'Array range dimensionality mismatch in parameter passing or '
        f'assignment: {range_l} elements instead of {range_r}')

  def mismatch_fail():
    raise TypeMismatch(f'Found {actual} where type {expected} was expected')

  # Main type comparison logic.
  match expected:
    # Union of types.
    case Union(types=types):
      return any(
          check_assignment_or_parameter_compatibility(t, actual) for t in types)

    # 1-D array.
    case mupas_types.Array1d:
      return check_assignment_or_parameter_compatibility(
          Array1d.from_array1d(expected), actual)

    case Array1d(index_typeinfo=l_index, value_typeinfo=l_value):
      match actual:
        case mupas_types.Array1d(index_typeinfo=r_index,
                                 value_typeinfo=r_value):
          # 1-D array assignment to 1-D array.
          check_assignment_or_parameter_compatibility(l_value, r_value)
          if l_index is not None: ord_range_check(l_index, r_index)
        case mupas_types.ScalarNumber:
          # Scalar assignment to 1-D array.
          check_assignment_or_parameter_compatibility(l_value, actual)
        case _:
          # Anything else to 1-D array.
          mismatch_fail()

    # 2-D array.
    case mupas_types.Array2d:
      return check_assignment_or_parameter_compatibility(
          Array1d.from_array2d(expected), actual)

    case Array2d(row_index_typeinfo=l_row,
                 col_index_typeinfo=l_col,
                 value_typeinfo=l_value):
      match actual:
        case mupas_types.Array2d(row_index_typeinfo=r_row,
                                 col_index_typeinfo=r_col,
                                 value_typeinfo=r_value):
          # 2-D array assignment to 2-D array.
          check_assignment_or_parameter_compatibility(l_value, r_value)
          if l_row is not None: ord_range_check(l_row, r_row)
          if l_col is not None: ord_range_check(l_col, r_col)
        case mupas_types.ScalarNumber:
          # Scalar assignment to 2-D array.
          check_assignment_or_parameter_compatibility(l_value, actual)
        case _:
          # Anything else to 2-D array.
          mismatch_fail()

    # String.
    case mupas_types.String:
      return check_assignment_or_parameter_compatibility(
          String.from_string(expected), actual)

    case String(length=l_len):
      match actual:
        case mupas_types.String(length=r_len):
          # String assignment to a string.
          if l_len is not None and r_len > l_len: warnings.warn(
              f'Using a string of size {r_len} where a string of size {l_len} '
              f'is expected may cause errors if the string is longer than '
              f'{l_len} characters')
        case _:
          # Anything else to string.
          mismatch_fail()

    # Scalar numbers of all types.
    case mupas_types.ScalarNumber:
      # Any non-scalar to scalar assignment.
      if not isinstance(actual, mupas_types.ScalarNumber):
        mismatch_fail()
      # Real to anything not real.
      elif isinstance(actual, mupas_types.Real):
        if not isinstance(expected, mupas_types.Real): warnings.warn(
            f'Using an {actual} value in a {expected} context could result in '
            'ordinal values no longer being made of integers; consider an '
            'explicit conversion or cast')
      # Ordinal to ordinal.
      elif all(isinstance(x, mupas_types.Ordinal) for x in (expected, actual)):
        elb, eub = expected.lower_bound, expected.upper_bound
        alb, aub = actual.lower_bound, actual.upper_bound
        if alb < elb or aub > eub: warnings.warn(
            f'Using a {actual} (which can have values in {alb}..{aub}) in a '
            f'{expected} (which can have values in {elb}..{eub}) context; '
            'consider an explicit conversion or cast.')


class TypeMismatch(RuntimeError):
  """A custom error for type mismatches, making them easier to catch."""
