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

import contextlib
import dataclasses
import math
import textwrap

import mupas_types
import mupas_resources
import pascal_parser

from typing import Generic, Iterator, Mapping, Optional, Protocol, Sequence
from typing import TypeVar


T = TypeVar('T')


_UNBOUND = '<unbound>'


### Generic classes for nested scopes.###


class ScopeProtocol(Protocol[T]):
  name: str
  path: str
  parent: Optional['ScopeProtocol[T]']
  bindings: dict[str, T]

  def __setitem__(self, name: str, item: T):
    ...

  def __getitem__(self, name: str) -> T:
    ...

  def __contains__(self, name: str) -> bool:
    ...

  def __delitem__(self, name: str):
    ...

  def itempath(self, name: str) -> str:
    ...

  def _itempath_rec(self, name: str, original_path: str) -> str:
    ...

  def itemhops(self, name: str) -> int:
    ...

  def _itemhops_rec(self, name: str, original_path: str) -> int:
    ...

  @contextlib.contextmanager
  def substitute(self, **kwargs: T) -> Iterator:
    ...


class BiScopeProtocol(ScopeProtocol[T]):
  children: dict[str, 'BiScopeProtocol[T]']

  @contextlib.contextmanager
  def nest(self, name: str) -> Iterator['BiScopeProtocol[T]']:
    ...

  def add_child(self, name: str, child: 'BiScopeProtocol[T]'):
    ...

  def del_child(self, name: str):
    ...

  def get_scope(self, path: str) -> 'BiScopeProtocol[T]':
    ...


class Scope(ScopeProtocol[T]):
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
  parent: Optional[ScopeProtocol[T]]
  bindings: dict[str, T]

  def __init__(self, name: str, parent: Optional[ScopeProtocol] = None):
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
    """Retrieve the Unix-like path to an item in this scope."""
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
    """If name is in the Nth parent scope to this scope, return N."""
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

  @contextlib.contextmanager
  def substitute(self, **kwargs: T):
    """A context where some bindings are temporarily substituted as supplied."""
    old_bindings = self.bindings
    self.bindings = dict(self.bindings, **kwargs)
    try:
      yield
    finally:
      self.bindings = old_bindings


class BiScope(Scope[T], BiScopeProtocol[T]):
  """A generic scope class with bidirectional references.

  A BiScope is just like a Scope, except parent scopes have a dict that
  collects all of their child scopes.

  Attributes:
    children: Dict of child scopes, indexed by their names.
  """
  children: dict[str, BiScopeProtocol[T]]

  def __init__(self, name: str, parent: Optional[BiScopeProtocol[T]] = None):
    super().__init__(name, parent)
    self.children = {}
    if parent is not None: parent.add_child(name, self)

  @contextlib.contextmanager
  def nest(self, name: str) -> Iterator[BiScopeProtocol[T]]:
    """Creates a context where a new BiScope uses this scope as a parent."""
    bi_scope: BiScopeProtocol[T] = BiScope(name=name, parent=self)
    try:
      yield bi_scope
    finally:
      self.del_child(name)

  def add_child(self, name: str, child: BiScopeProtocol[T]):
    """Add a child to this scope. Does not ensure this object is its parent."""
    if name in self.children: raise KeyError(
      f'{self.path} already has a child scope called {name}')
    self.children[name] = child

  def del_child(self, name: str):
    """Deletes a child from this scope."""
    if name not in self.children: raise KeyError(
      f'{self.path} has no child scope called {name} to delete')
    del self.children[name]

  def get_scope(self, path: str) -> BiScopeProtocol[T]:
    """Retrieve a scope by its path. Path must start with this scope."""
    # This implementation is a little cheesy. All paths passed as path will be
    # treated as relative paths even if they begin with '/'.
    parts = [p for p in path.split('/') if p]
    try:
      if parts.pop(0) != self.name: raise KeyError
      scope: BiScopeProtocol[T] = self
      for p in parts:
        scope = scope.children[p]
      return scope
    except KeyError:
        raise KeyError(f'No scope {path} within {self.name}') from None


### Instantiations of nested scopes ###


TypeScope = Scope[mupas_types.Type]
TypeScopeProtocol = ScopeProtocol[mupas_types.Type]


### Symbol table values ###


@dataclasses.dataclass
class Symbol:
  """Superclass for symbol table entry values.

  These values hold bookeeping information about symbol bindings.
  """
  definition: pascal_parser.AstNode

  def __str__(self) -> str:
    return self.__class__.__name__  # A fallback that limits clutter.


@dataclasses.dataclass
class VariableSymbol(Symbol):
  """Some symbols are bound to variables which refer to means of storage."""
  typeinfo: mupas_types.Type
  storage: Optional[mupas_resources.Value] = None

  def __str__(self) -> str:
    storage = _UNBOUND if self.storage is None else str(self.storage)
    return f'{storage:19} {self.typeinfo}'


@dataclasses.dataclass
class ReferenceSymbol(Symbol):
  """Some symbols are references to values in other scopes (support TBA)."""
  typeinfo: mupas_types.Type


@dataclasses.dataclass
class SubroutineSymbol(Symbol):
  """Some symbols are bound to muPas subroutines."""
  typeinfo: mupas_types.Subroutine
  frame: Optional[mupas_resources.Frame] = None

  def __str__(self) -> str:
    return str(self.typeinfo)


@dataclasses.dataclass
class ConstantIntegerSymbol(Symbol):
  """Some symbols are bound to integer-valued constants."""
  typeinfo: mupas_types.Integer
  value: int

  def __init__(self, definition: pascal_parser.AstNode, value: int):
    super().__init__(definition)
    self.typeinfo = mupas_types.Integer()
    self.value = value

  def __str__(self) -> str:
    return f'{self.value:<19} CONST {self.typeinfo}'


@dataclasses.dataclass
class ConstantRealSymbol(Symbol):
  """Some symbols are bound to real-valued constants."""
  typeinfo: mupas_types.Real
  value: float

  def __init__(self, definition: pascal_parser.AstNode, value: float):
    super().__init__(definition)
    self.typeinfo = mupas_types.Real()
    self.value = value

  def __str__(self) -> str:
    return f'{self.value:<19} CONST {self.typeinfo}'


@dataclasses.dataclass
class ConstantStringSymbol(Symbol):
  """Some symbols are bound to string constants. (Some in storage.)"""
  typeinfo: mupas_types.String
  index: int
  storage: Optional[mupas_resources.Value] = None

  def __str__(self) -> str:
    storage = _UNBOUND if self.storage is None else str(self.storage)
    return f'{storage:19} CONST {self.typeinfo}'


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


SymbolScope = BiScope[Symbol]
SymbolScopeProtocol = BiScopeProtocol[Symbol]


### Utilities ###


def symbol_table_text(symbols: SymbolScopeProtocol) -> Sequence[str]:
  """Produce a printable representation of a symbol table.

  The dumped representation will show all of the entries in the symbol table,
  organised by the context in which they are defined. The format of the table
  isn't specified, is target-specific, and is intended for humans to read,
  although machine parsing may be possible.

  Args:
    symbols: A symbol table.

  Returns:
    A sequence of strings (without newlines) that together represent the
    contents of the symbol table.
  """
  text = [f'{symbols.path}:']

  # Collect extensions if any are defined in this scope.
  extensions = [k for k, v in symbols.bindings.items()
                if isinstance(v, ExtensionSymbol)]
  if extensions:
    text.append('  Extensions: ')
    text.extend(
        [f'    {s}' for s in textwrap.wrap(', '.join(extensions), width=74)])

  # Determine the length of the longest symbol name and the width we'll use for
  # the symbol name field for each binding.
  if symbols.bindings:
    max_name_len = max(len(k) for k, v in symbols.bindings.items()
                       if not isinstance(v, ExtensionSymbol))
  else:
    max_name_len = 0  # No symbols are bound local to this scope!
  name_width = max(12, 4 * math.ceil((max_name_len + 2) / 4))

  # Now list the non-extension bindings defined in this scope.
  for k in sorted(symbols.bindings):
    v = symbols.bindings[k]
    if not isinstance(v, ExtensionSymbol):
      text.append(f'  {k.ljust(name_width)}{v}')

  # Recursively append the representations of child scopes.
  for kid in sorted(symbols.children):
    text.extend(symbol_table_text(symbols.children[kid]))

  return text
