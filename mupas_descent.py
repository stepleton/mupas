"""Utilities for recursive descent into muPas parse trees.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

Functions in this module include `scan`, a fully-general "industrial strength"
recursive-descent helper, and `depth_first`, a less-featureful variant that
may be adequate for some uses. Code in `mupas_analyses` makes use of these
helpers extensively; code generating routines may do so as well if they wish
but aren't required to.
"""

import pascal_parser

from typing import Callable, Generic, Protocol, TypeVar


TState = TypeVar('TState', contravariant=True)
TResult = TypeVar('TResult', covariant=True)
TScanTodos = list[
    tuple['Callback[TResult, TState]', pascal_parser.AstNode, TState]]


class Callback(Protocol[TResult, TState]):
  def __call__(self,
               ast: pascal_parser.AstNode,
               state: TState,
               todos: TScanTodos) -> TResult:
    """A function signature for callbacks used by scan.

    Args:
      ast: Current parse tree node encountered in the traversal.
      state: Arbitrary state data passed this DescentCallback.
      todos: Mutable list of parse tree nodes to traverse next, in reverse
          order, along with the callbacks to use to traverse them and an
          arbitrary state item to pass to those callbacks. Typically, any one
          callback will need to add entries to this list based on the contents
          of `ast` in order for the traversal to include children of `ast`.
          Appending those entries to the end of the list yields a depth-first
          search; for a breadth-first search, prepend. Empty the list (`del
          todos[:]`) to cease the traversal.

    Returns:
      An arbitrary value. See scan documentation for more details.
    """
    ...


def scan(callback: Callback[TResult, TState],
         ast: pascal_parser.AstNode, state: TState) -> TResult:
  """General-purpose parse tree scanner.

  Invokes callbacks (see `Callback`) on nodes of a parse tree that have been
  accumulated into an internal double-ended queue (let's call it `todos`).
  Nodes and callbacks are popped off of the end of `todos`. The callbacks can
  add new node-callback pairs to `todos` in order to visit new places in the
  parse tree. If a callback adds all children of a node, then a complete parse
  tree traversal is achieved.

  The `Callback` documentation contains useful additional details on how
  callbacks should work.

  Args:
    callback: Callback to apply to the starting node `ast`.
    ast: Starting node for the scan.
    state: State to pass to the callback as it processes `ast`.

  Returns:
    Whatever was returned by the last callback to be called.
  """
  todos: TScanTodos = [(callback, ast, state)]
  while todos:
    next_callback, next_ast, next_state = todos.pop()
    result = next_callback(next_ast, next_state, todos)
  return result


class Stop(Exception, Generic[TResult]):
  """A short-circuiting exception for simplified scanners.

  Callbacks used with any of the simplified scanning functions defined below
  can raise this exception if they would like to abort the scan early. The
  value passed to the interrupt constructor will be returned to the scan's
  caller.
  """
  payload: TResult

  def __init__(self, payload: TResult):
    """Initialise with a value that the scan should return."""
    self.payload = payload


def depth_first(callback: Callable[[pascal_parser.AstNode, TState], TResult],
                ast: pascal_parser.AstNode, state: TState) -> TResult:
  """Simplified depth-first scanner.

  A simplification of `scan` that automatically carries out a depth-first
  traversal of a parse tree. The callback is called prior to descending
  into the children of a tree node.

  WARNING: For now, children of a tree node are not evaluated in any
  particular order!

  If a callback wishes to cancel the traversal, it can raise a Stop exception.
  The value passed within the Stop is returned to the caller of `depth_first`.
  Otherwise, the value returned by the final call to callback in the traversal
  is returned.

  Args:
    callback: Called for each AST node in the parse tree. The second argument
        is the `state` argument to this function.
    ast: Root of the parse tree to traverse.
    state: State object to pass to all calls of `callback`.

  Returns:
    As described above.
  """

  def callback_wrapper(ast: pascal_parser.AstNode,
                       state: TState, todos: TScanTodos) -> TResult:
    """Adapts the callback to a more complicated `scan` callback."""
    try:
      result = callback(ast, state)  # Try calling the user's callback.
    except Stop as s:                # If the callback wants to abort the scan,
      del todos[:]                   # Clear all next items to scan.
      return s.payload               # type: ignore  # Return exception payload.
    else:                            # But if the callback returned normally:
      # Schedule iteration into all child nodes of this parse tree node. We say
      # reversed because `children()` occasionally returns a list of things
      # contained by an AST node (e.g. the statements within a block), and we'd
      # like scan() to process them in order.
      todos.extend((callback_wrapper, kid, state)
                   for kid in reversed(children(ast)))
      return result                  # Return what the user's callback did.

  return scan(callback_wrapper, ast, state)


### Utilities ###


def children(ast: pascal_parser.AstNode) -> list[pascal_parser.AstNode]:
  """Retrieve all parse tree node children of this parse tree node."""
  kids : list[pascal_parser.AstNode] = []
  for sub_ast in ast.__dict__.values():
    if isinstance(sub_ast, (tuple, list)):
      kids.extend(n for n in sub_ast if isinstance(n, pascal_parser.AstNode))
    elif isinstance(sub_ast, pascal_parser.AstNode):
      kids.append(sub_ast)
  return kids
