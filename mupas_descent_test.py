"""Tests for the mupas_descent module.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.
"""

import unittest

import mupas_descent
import preprocessor
import pascal_parser

from typing import Callable


# Source code used by all of the tests in this module. You'll want to check
# what the tests do before you change this code.
SOURCE_TEXT = """\
    PROGRAM Beans;
    CONST
      kItem = 'This program is about beans';
    VAR
      foo: Integer;
    BEGIN
      WriteLn('Beans!');
      foo := -10000000 * (-102928) DIV 32763;
      IF TRUE THEN BEGIN
        WriteLn('Eat lots of beans!');
        IF FALSE THEN WriteLn('Eat magical beans.')
      END ELSE BEGIN
        WriteLn('Do not eat beans.')
      END
    END."""


def parse(source_text: str) -> pascal_parser.AstNode:
  """Convert source text into an abstract syntax tree."""
  return pascal_parser.parse(preprocessor.preprocess(source_text)[0])


class MupasDescentTest(unittest.TestCase):
  """Test harness for testing the mupas_descent module."""

  def test_depth_first(self):
    """Basic operation of the depth-first simplified scanner."""

    # This callback can be used with the depth-first scanner to count the
    # number of string constants in a syntax tree. The count must be boxed
    # into a list so that we can mutate it.
    def callback(ast: pascal_parser.AstNode, state: list[int]) -> int:
      if isinstance(ast, (pascal_parser.QuotedConstant,
                          pascal_parser.ConstantQuotedConstant)):
        state[0] += 1
      return state[0]

    ast = parse(SOURCE_TEXT)
    self.assertEqual(mupas_descent.depth_first(callback, ast, [0]), 5)

  def test_depth_first_stop(self):
    """Interruption of depth-first scanning with a Stop exception."""

    # Like the callback above, but stops if it encounters a six-character-long
    # string constant and forces the scan to return the nonsense value -6.
    def callback(ast: pascal_parser.AstNode, state: list[int]) -> int:
      if isinstance(ast, (pascal_parser.QuotedConstant,
                          pascal_parser.ConstantQuotedConstant)):
        state[0] += 1
        if ast.code_length == 8:
          raise mupas_descent.Stop(-6)
      return state[0]

    ast = parse(SOURCE_TEXT)
    self.assertEqual(mupas_descent.depth_first(callback, ast, [0]), -6)

  def test_scan(self):
    """A more complicated example that demonstrates scan()."""

    # Useful types for the following.
    State = Callable[[int], int]
    Todos = list[tuple[mupas_descent.Callback[int, State],
                       pascal_parser.AstNode, State]]

    # This callback recurses into the parse tree. As soon as it encounters any
    # assignment expression, it deploys other callbacks that evaluate and
    # return the expression's value. (We assume that this is an integer
    # expression.
    def look_for_assigment(ast: pascal_parser.AstNode, state: State,
                           todos: Todos) -> int:
      if isinstance(ast, pascal_parser.StatementAssignment):
        del todos[:]  # We've got an assingnment; stop searching for one.
        todos.append((eval_int_expression, ast.value, state))
      else:
        kids = mupas_descent.children(ast)
        todos.extend((look_for_assigment, kid, state) for kid in reversed(kids))
      # This return value will not be used; unless no assignment expression is
      # is found, this callback will not be the last one called by scan().
      return -1

    # This more complicated callback is a interpreter for (limited) arithmetic
    # expressions (currently only unary negation, multiplication, and integer
    # division). In a way, it's a bit like a compiler: the state that it passes
    # are nested callables that evaluate parts of the expression when called
    # with an integer value. You could think of these as compiled programs.
    def eval_int_expression(ast: pascal_parser.AstNode, state: State,
                            todos: Todos) -> int:
      if isinstance(ast, pascal_parser.ExpressionLeaf):
        assert isinstance(ast.value, pascal_parser.UnsignedInteger)
        return state(ast.value.number)

      elif isinstance(ast, pascal_parser.ExpressionUnary):
        assert ast.op == pascal_parser.UnaryOp.NEGATE
        todos.append((eval_int_expression, ast.expression, lambda x: state(-x)))
        return -1  # Unused; will not be the last callback called by scan()

      elif isinstance(ast, pascal_parser.ExpressionBinary):
        left_side : int = 0   # This state callable evaluates the expression's
        def state_left(val):  # left side and stores the result in left_side.
          nonlocal left_side  # State callables for the expression's right side
          left_side = val     # can use that value to compute their result.
          return -1  # Unused; won't be called by the last scan() callback.
        if ast.op == pascal_parser.BinaryOp.MULTIPLY:
          state_right = lambda x: state(left_side * x)
        elif ast.op == pascal_parser.BinaryOp.DIVIDE_TO_INT:
          state_right = lambda x: state(left_side // x)
        else:
          assert False, 'We can only multiply and divide.'
        # We append to todos in this order so that we bind left_side before the
        # state_right callable attempts to use that binding.
        todos.append((eval_int_expression, ast.expression_right, state_right))
        todos.append((eval_int_expression, ast.expression_left, state_left))
        return -1  # Unused; will not be the last callback called by scan()

      return -1  # For mypy.

    ast = parse(SOURCE_TEXT)
    self.assertEqual(
        mupas_descent.scan(look_for_assigment, ast, lambda x: x), 31415927)
