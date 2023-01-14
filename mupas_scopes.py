"""Variable and type scopes for muPas.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

A scope is a mapping with an ancestry. A lookup within a scope gets passed
along to a parent scope if it can't be resolved locally. This approach to
binding resolution mimics the rules for resolving names within Pascal programs.

Besides generic superclasses for scopes and for "BiScopes" (these being scopes
who know who their "child" scopes are), this module contains specialised
subclasses: type scopes and symbol scopes. Type scopes are used to organise
scoped type definitions in muPas programs (think of how a subroutine can
declare its own types under its `TYPE`), while symbol scopes are used to
organise the symbols that refer locally and globally to constants, variables,
subroutines, and "extensions". (Extensions are subroutine-like language
extensions which are defined separately for each architecture: refer to code
generation code for more details.)

For symbol scopes, this module also defines records that store information for
various types of symbols. The kinds of symbols that correspond to information
kept in memory (so, variables and string constants) include details of where
those values are stored, but only after these details have been calculated by
the `add_static_resources` and `add_stack_resources` functions in
`mupas_analyses`.
"""

import dataclasses

import mupas_types
import mupas_resources
import pascal_parser

from typing import Generic, Optional, TypeVar


T = TypeVar('T')
TScope = TypeVar('TScope', bound='Scope')
TBiScope = TypeVar('TBiScope', bound='BiScope')


### Generic classes for nested scopes.###


class Scope(Generic[T]):
  """A generic base class for scopes.

  Attributes:
    name: Name for this scope --- if you think of the scope as a directory in
        a filesystem tree, then this name is the name of the directory. Must
        not include the '/' character.
    path: A '/'-delimited string of enclosing scopes starting from the root
        scope and ending in this scope. In the filesystem analogy, the "full
        path" to this directory.
    parent: Enclosing scope, or None for the root scope.
    bindings: Mappings from names in this scope to T items.
  """
  name: str
  path: str
  parent: Optional[TScope]
  bindings: dict[str, T]

  def __init__(self, name: str, parent: Optional[TScope] = None):
    if '/' in name: raise ValueError(
        f'Scope name "{name}" includes illegal character \'/\'')
    self.name = name
    self.path = f'/{name}' if parent is None else f'{parent.path}/{name}'
    self.parent = parent
    self.bindings = {}

  def __setitem__(self, name: str, item: T):
    if name in self.bindings: raise KeyError(
        f'Symbol {name} is already bound in {self.path}')
    self.bindings[name] = item

  def __getitem__(self, name: str) -> T:
    try:
      return self._getitem_rec(name, self.path)
    except KeyError as e:
      raise e.with_traceback(None)

  def _getitem_rec(self, name:str, original_path: str) -> T:
    if name in self.bindings:
      return self.bindings[name]
    elif self.parent is not None:
      return self.parent[name]
    else:
      raise KeyError(f'Symbol {name} is unbound in {original_path}')

  def __contains__(self, name: str) -> bool:
    return name in self.bindings or (
        self.parent is not None and name in self.parent)

  def __delitem__(self, name: str):
    try:
      del self.bindings[name]
    except KeyError:
      raise KeyError(
          f'Symbol {name} is unbound in {self.path}' if name not in self else
          f"Symbol {name} is in a scope enclosing {self.path}; can't delete"
      ) from None

  def itempath(self, name: str) -> str:
    try:
      return self._itempath_rec(name, self.path)
    except KeyError as e:
      raise e.with_traceback(None)

  def _itempath_rec(self, name: str, original_path: str) -> str:
    if name in self.bindings:
      return f'{self.path}/{name}'
    elif self.parent is None:
      raise KeyError(f'Symbol {name} is unbound in {original_path}')
    else:
      return self.parent._itempath_rec(name, original_path)

  def itemhops(self, name: str) -> int:
    try:
      return self._itemhops_rec(name, self.path)
    except KeyError as e:
      raise e.with_traceback(None)

  def _itemhops_rec(self, name: str, original_path: str) -> int:
    if name in self.bindings:
      return 0
    elif self.parent is None:
      raise KeyError(f'Symbol {name} is unbound in {original_path}')
    else:
      return 1 + self.parent._itemhops_rec(name, original_path)


class BiScope(Scope[T]):
  """A generic scope class with bidirectional references.

  A BiScope is just like a Scope, except parent scopes have a dict that
  collects all of their child scopes.

  Attributes:
    children: Dict of child scopes, indexed by their names.
  """
  children: dict[str, TBiScope]

  def __init__(self, name: str, parent: Optional[TBiScope] = None):
    super().__init__(name, parent)
    self.children = {}
    if parent is not None: parent.add_child(name, self)

  def add_child(self, name: str, child: TBiScope):
    """Add a child to this scope. Does not ensure this object is its parent."""
    if name in self.children: raise KeyError(
      f'{self.path} already has a child scope called {name}')
    self.children[name] = child

  def get_scope(self, path: str):
    """Retrieve a scope by its path. Path must start with this scope."""
    # This implementation is a little cheesy. All paths passed as path will be
    # treated as relative paths even if they begin with '/'.
    parts = [p for p in path.split('/') if p]
    try:
      if parts.pop(0) != self.name: raise KeyError
      scope = self
      for p in parts:
        scope = scope.children[p]
      return scope
    except KeyError:
        raise KeyError(f'No scope {path} within {self.name}') from None


### Instantiations of nested scopes ###


class TypeScope(Scope[mupas_types.Type]):
  """Nested table of types in muPas programs."""


### Symbol table values ###


@dataclasses.dataclass
class Symbol:
  """Superclass for symbol table entry values.

  These values hold bookeeping information about symbol bindings.
  """
  definition: pascal_parser.AstNode


@dataclasses.dataclass
class VariableSymbol(Symbol):
  """Some symbols are bound to variables which refer to means of storage."""
  typeinfo: mupas_types.Type
  storage: Optional[mupas_resources.Value] = None


@dataclasses.dataclass
class ReferenceSymbol(Symbol):
  """Some symbols are references to values in other scopes (support TBA)."""
  typeinfo: mupas_types.Type


@dataclasses.dataclass
class SubroutineSymbol(Symbol):
  """Some symbols are bound to muPas subroutines."""
  typeinfo: mupas_types.Subroutine
  frame: Optional[mupas_resources.Frame] = None


@dataclasses.dataclass
class ConstantIntegerSymbol(Symbol):
  """Some symbols are bound to integer-valued constants."""
  typeinfo: mupas_types.Integer
  value: int

  def __init__(self, definition: pascal_parser.AstNode, value: int):
    super().__init__(definition)
    self.typeinfo = mupas_types.Integer()
    self.value = value


@dataclasses.dataclass
class ConstantRealSymbol(Symbol):
  """Some symbols are bound to real-valued constants."""
  typeinfo: mupas_types.Real
  value: float

  def __init__(self, definition: pascal_parser.AstNode, value: float):
    super().__init__(definition)
    self.typeinfo = mupas_types.Real()
    self.value = value


@dataclasses.dataclass
class ConstantStringSymbol(Symbol):
  """Some symbols are bound to string constants. (Some in storage.)"""
  typeinfo: mupas_types.String
  index: int
  storage: Optional[mupas_resources.Value] = None


@dataclasses.dataclass
class ExtensionSymbol(Symbol):
  """Some symbols are bound to target-specific language extensions.

  Extensions are symbols which are used in muPas programs like subroutine
  calls. During code generation, the code generator uses the extension symbol
  and any argument expressions to generate target-specific code. As an example,
  the Tektronix 4050 BASIC target uses extensions extensively to implement
  Pascal standard procedures and functions by invoking their BASIC analogues.

  Because extensions are target-specific, there's not much we can do here
  besides be an empty superclass. 
  """


class SymbolScope(BiScope[Symbol]):
  """Nested symbol table for muPas programs."""
