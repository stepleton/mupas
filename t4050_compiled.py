"""Compiled muPas code fragments for statements and expressions.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

The data structures defined in this module hold fragments of code generated
internally to the `t4050_generator` module. Code generation is, in a sense, a
matter of generating small fragments and then coalescing them into bigger and
bigger ones.
"""
import dataclasses
import itertools

import mupas_types

from typing import Sequence


@dataclasses.dataclass
class Statement:
  """Contains compiled code that executes a statement.

  Attributes:
    code: Compiled BASIC statements that implement a sequence of muPas
        statements.
    stack_growth: How many new entries will be added to the stack (beyond the
        original size of the stack frame) when the code in `code` executes?
  """
  code: list[str]
  stack_growth: int = 0


@dataclasses.dataclass
class Expression:
  """Contains compiled code that calculates an expression.

  Attributes:
    typeinfo: The muPas type of the original expression.
    access: A BASIC expression that, when evaluated, yields the value of the
        expression. This may refer to the result of the code listed in the
        `compute` attribute. This code is expected to be used inline in larger
        BASIC expressions and statements. We can't say how often this
        expression will be evaluated, so it shouldn't grow (or even alter) the
        stack. In principle, this expression should NOT have side effects when
        evaluated, though that may be permissible in some places as a matter
        of taste (e.g. advancing the state of a pseudorandom number generator).
        If there are side-effects that absolutely must occur as a consequence
        of evaluating an expression (that is, they must not be optimised away
        under any circumstances), they ought to be carried out by code in the
        `compute` attribute.
    compute: BASIC code that should be executed first in order to calculate the
        value of a muPas expression. This code makes sense only in the context
        of surrounding code: for example, if the expression is a function call,
        this code would prepare the parameters and then call the function. Some
        expressions will not need any code for this attribute: for example, a
        literal integer value doesn't require any computation and will only
        need to specify the `access` attribute.
    stack_growth: How many new entries had to be added to the stack (beyond the
        original size of the stack frame) by the code in the `compute`
        attribute in order to make it possible to evaluate the expression?
    can_be_lhs: Whether the BASIC code for the expression can be on the
        left-hand side of a BASIC assignment statement. Such expressions
        include both unqualified variables and variables qualified by array
        indices, but NOT function names (for setting return values) --- those
        are handled differently than expressions (and their values can't be
        read anyway).
    is_unqualified_variable: Whether the BASIC code for the expression names a
        BASIC variable without qualifying it (e.g. by indexing an element, if
        the variable is a string or an array). Note that unqualified string
        constant expressions that aren't string literals (i.e. unqualified
        references to strings defined in a CONST definition) do receive a True
        value here, although `can_be_lhs` is False in those cases.
    is_literal: Whether the BASIC code for the expression is a BASIC literal.
  """
  # Ideas for the future:
  # - Carry information necessary for more efficient use of parentheses in
  #   compiled code.
  # - Identify whether an array expression is a leaf --- those that aren't
  #   can't be used as intermediate values in larger expressions.
  # - Add an expression type for implicit type casting.
  # - Identify whether an expression must be used, e.g. POS (gotta do
  #   something with it).
  # - Identify whether an expression must be assigned, e.g. an array binary
  #   expression.
  typeinfo: mupas_types.Type
  access: str
  compute: list[str] = dataclasses.field(default_factory=list)
  stack_growth: int = 0

  can_be_lhs: bool = False
  is_unqualified_variable: bool = False

  is_literal: bool = False


def chain_statements(statements: Sequence[Statement]) -> Statement:
  """Combine a sequence of Statements into one big Statement."""
  return Statement(
      code=list(itertools.chain(*(s.code for s in statements))),
      stack_growth=sum(s.stack_growth for s in statements))
