"""muPas extension base classes for the Tek 4050-series BASIC generator.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This module establishes base classes for muPas extensions for the Tektronix
4050-series BASIC target. Here, all extensions are invoked in ways that
resemble subroutine calls, so behind the scenes (i.e. in subclasses of the
classes defined here), extensions are essentially code-generating functions
that take a number of compiled expressions and return a compiled function or a
compiled statement.

There are three types of extensions:
  - Function-like extensions: extensions that behave like functions. These
    extensions generate compiled expressions.
  - Procedure-like extensions: extensions that behave like procedures. These
    extensions generate compiled statements.
  - Exit extensions: this is an implementation detail of how the code
    generator for Tek 4050-series BASIC handles the Exit keyword.

Extensions are responsible for doing all the same work that "normal" code
generation does: they must generate code that makes certain that the
extension's arguments are computed, and they must correctly calculate how much
the stack grows when that code and when the extension's own code runs.
Extensions are also responsible for all of their own type checking.

Unlike ordinary muPas subroutines, which can't mutate their arguments (yet),
extensions often can. Check the attributes of the `t4050_compiled.Expression`
values passed to the extension: if `can_be_lhs` or `is_mutable_variable` are
true, depending on your needs, you may be able to use the expression's
`.access` member to get the name of a bare variable or an array element that
you can modify.

Extensions don't really have an obvious way of allocating space on the stack
for themselves if they need it. The safest thing to do is probably to define a
subroutine that declares local variables and then passes them to the extension.

For lots of examples of extensions for the Tek 4050-series BASIC target, see
the `t4050_stdlib_extensions` module.
"""

import dataclasses

import mupas_scopes
import pascal_parser
import t4050_compiled

from typing import Callable


_ExtensionFunction = Callable[
    [list[t4050_compiled.Expression]], t4050_compiled.Expression]
_ExtensionProcedure = Callable[
    [list[t4050_compiled.Expression]], t4050_compiled.Statement]


@dataclasses.dataclass
class ExtensionFunctionSymbol(mupas_scopes.ExtensionSymbol):
  """Specialisation of ExtensionSymbol to functions, for 4050-series BASIC."""
  extension: _ExtensionFunction

  def __init__(self, extension: _ExtensionFunction):
    super().__init__(definition=pascal_parser.Extension())
    self.extension = extension


@dataclasses.dataclass
class ExtensionProcedureSymbol(mupas_scopes.ExtensionSymbol):
  """Specialisation of ExtensionSymbol to statements, for 4050-series BASIC."""
  extension: _ExtensionProcedure

  def __init__(self, extension: _ExtensionProcedure):
    super().__init__(definition=pascal_parser.Extension())
    self.extension = extension


@dataclasses.dataclass
class ExtensionExitSymbol(mupas_scopes.ExtensionSymbol):
  """A value for binding Exit in the root symbol table.

  ExtensionExitSymbol is not like ordinary extensions: instead, it's simply a
  marker that lets t4050_generator.sta_procedure know that the user has
  called Exit. When sta_procedure sees this marker, it generates custom code
  that jumps to the current block's exit point.
  """
  def __init__(self):
    super().__init__(definition=pascal_parser.Extension())
