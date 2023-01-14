"""Various analyses of muPas parse trees.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

The analyses in this file are essentially applied to muPas programs in the
order listed. After parsing,

   check_parse_tree

flags more advanced Pascal features that muPas doesn't support; next

   get_symbols

derives a symbol table from the program but without any storage allocated for
variables. Before this can be done,

   get_call_graph

and

   get_reverse_transitive_call_graph

gathers information that's useful for deciding when storage resources can be
reused. This information is provided to

   add_static_resources

which also receives an additional architecture-specific resource allocation
object so that it can assign some symbols to static resources, after which

   add_stack_resources

assigns the remaining variable and parameter resources to entries on an
abstracted stack (see the `mupas_stack` module.
"""

import mupas_descent
import mupas_stack
import mupas_static
import mupas_scopes
import mupas_types
import pascal_parser

from typing import Any, Collection, Callable, Iterable, Mapping, Sequence, Optional


# Used by get_type: a mapping from parse tree node types to symbol table
# values for certain elementary types.
_SIMPLE_TYPE_MAPPING : Mapping[type, mupas_types.Type] = {
    pascal_parser.TypeBoolean: mupas_types.Boolean(),
    pascal_parser.TypeChar: mupas_types.Char(),
    pascal_parser.TypeInteger: mupas_types.Integer(),
    pascal_parser.TypeLongint: mupas_types.Longint(),
    pascal_parser.TypeReal: mupas_types.Real(),
}


def check_parse_tree(ast: pascal_parser.Program):
  """Checks whether a Pascal parse tree obeys muPas simplifying assumptions.

  Current assumptions checked are:
    - No use of SETs, RECORDs, FILEs, or pointers.
  There are other simplifications in muPas, but these are checked elswhere.

  Args:
    ast: A parse tree --- must be a Program node.

  Raises:
    RuntimeError: a simplifying assumption has been violated.
  """
  if ast.parameters is not None: raise RuntimeError(
      'muPas programs cannot take parameters')
  if ast.uses_clause is not None: raise RuntimeError(
      'muPas programs cannot have a USES clause')
  block = ast.block  # Isolate the main program block for analysis.

  # muPas does not support sets, records, files, or pointers.
  def no_sets_records_files_or_pointers(ast: pascal_parser.AstNode, _):
    """For _descent: exception if unsupported types are found."""
    if isinstance(ast, (pascal_parser.TypeSet,
                        pascal_parser.TypeFile,
                        pascal_parser.TypePointer,
                        pascal_parser.TypeRecord,
                        pascal_parser.TypeClass)): raise RuntimeError(
        'muPas programs may not use pointers, FILEs, SETs, or RECORDs')

  mupas_descent.depth_first(no_sets_records_files_or_pointers, block, None)


def get_symbols(
    ast: pascal_parser.Program,
    quoted_constants: Sequence[str],
    extras: Optional[Mapping[str, mupas_scopes.Symbol]] = None,
) -> mupas_scopes.SymbolScope:
  """Construct a symbol table for a muPas program.

  The resulting nested symbol table will have entries for all the variable and
  constant symbols for the entire program. See comments within the function
  definition for insight into how it works.

  Args:
    ast: Parse tree for a muPas program.
    quoted_constants: Sequence of quoted constants from the program.
    extras: Additional symbols to add to the root symbol table.

  Returns:
    A nested symbol table containing all symbols declared in the program.
    This includes constant values and values of enumerated types.
  """

  # A note about how this function works: as it recurses into the parse tree,
  # it builds up definitions of types and symbols into two respective tables.
  # The helper functions and callbacks defined below simplify type definitions
  # eagerly so that they only refer to built-in muPas types --- so, for
  # example, if Foo is the range [0..3] and Bar is declared as an
  # ARRAY [0..5] OF Foo, then Bar will be recorded in the type table as an
  # ARRAY [0..5] OF [0..3].
  #
  # Variables, subroutine parameters, etc. are then associated with these
  # simplified type definitions.

  # Define useful types for recursive descent helpers.
  TypeScope, SymbolScope = mupas_scopes.TypeScope, mupas_scopes.SymbolScope
  DescentState = tuple[TypeScope, SymbolScope]
  DescentTodos = list[tuple[mupas_descent.Callback[None, DescentState],
                            pascal_parser.AstNode, DescentState]]

  # This helper retrieves an integer constant value from a Constant parse tree
  # node, either as a literal or as a value bound to a constant identifier.
  def get_integer_constant(ast: pascal_parser.Constant,
                           symbols: SymbolScope) -> int:
    match ast:
      case pascal_parser.ConstantSignedInteger(number=number):
        return number
      case pascal_parser.ConstantIdentifier(text=name, negated=negated):
        bound_item = symbols[name]
        if isinstance(bound_item, mupas_scopes.ConstantIntegerSymbol):
          return -bound_item.value if negated else bound_item.value
        else: raise RuntimeError(
          f'The non-integer constant {name} was referenced in a place where '
          'an integer constant was expected')
      case _: raise RuntimeError(
          'A non-integer constant was found in a place where an integer '
          'constant was expected')

  # This helper binds values to constant identifiers in constant declarations.
  # Life is easier for us because in Lisa Pascal, according to the grammar,
  # the right-hand sides of constant declarations can only be strings, numbers,
  # or identifiers of other constants.
  def bind_constant_declarations(ast: pascal_parser.ConstantDeclarationPart,
                                 state: DescentState, _):
    types, symbols = state
    for declaration in ast.declarations:
      name, value = declaration.text, declaration.value
      match value:
        case pascal_parser.ConstantQuotedConstant(index=index):
          length = len(quoted_constants[index])
          symbols[name] = mupas_scopes.ConstantStringSymbol(
              definition=ast, typeinfo=mupas_types.String(length), index=index)
        case pascal_parser.ConstantSignedInteger(number=number):
          symbols[name] = mupas_scopes.ConstantIntegerSymbol(
              definition=ast, value=number)
        case pascal_parser.ConstantSignedReal(number=number):
          symbols[name] = mupas_scopes.ConstantRealSymbol(
              definition=ast, value=number)
        case _: raise RuntimeError(
            f'An unexpected parse tree node of type {type(value)} is the '
            f'value for the constant {name} in the scope {symbols.path}')

  # For the most part, this helper converts the type specifications used
  # in type declarations, variable declarations, and subroutine parameter/
  # return declarations to the symbol metadata used in the symbol table.
  # Since it's used for type declarations (the definitions of types) and
  # also for other declarations (the applications of types), it's a little
  # bit messy: it has the ability to modify the symbol table to add the
  # numeric constants for enumerated types.
  #
  # The name argument is the symbol name and is used for error messages.
  def get_type(name: str, types: TypeScope, symbols: SymbolScope,
               the_type: pascal_parser.Type) -> mupas_types.Type:
    if isinstance(the_type, tuple(_SIMPLE_TYPE_MAPPING)):
      return _SIMPLE_TYPE_MAPPING[type(the_type)]
    match the_type:
      case pascal_parser.TypeSubrange(lower_bound=lower, upper_bound=upper):
        lo = get_integer_constant(lower, symbols)
        hi = get_integer_constant(upper, symbols)
        return mupas_types.IntegerSubrange(lo, hi)

      case pascal_parser.TypeString(stringsize=stringsize):
        return mupas_types.String(stringsize)

      case pascal_parser.TypeEnumerated(value_identifiers=value_identifiers):
        for i, identifier in enumerate(value_identifiers):
          symbols[identifier] = mupas_scopes.ConstantIntegerSymbol(
              definition=the_type, value=i)
        return mupas_types.Enumerated(identifiers=value_identifiers)

      case pascal_parser.TypeArray(index_types=ind_types, value_type=val_type):
        dims = len(ind_types)
        if dims == 1:
          return mupas_types.Array1d(
              index_typeinfo=get_type(
                  '<array index>', types, symbols, ind_types[0]),
              value_typeinfo=get_type(
                  '<array value type>', types, symbols, val_type))
        elif dims == 2:
          return mupas_types.Array2d(
              row_index_typeinfo=get_type(
                  '<array row index>', types, symbols, ind_types[0]),
              col_index_typeinfo=get_type(
                  '<array col index>', types, symbols, ind_types[1]),
              value_typeinfo=get_type(
                  '<array value type>', types, symbols, val_type))
        else: raise RuntimeError(
          f'muPas supports only 1-D and 2-D arrays, not {dims}-D arrays as '
          f'specified for the symbol {name} in the scope {symbols.path}')

      case pascal_parser.TypeIdentifier(text=name):
        return types[name]

      case _: raise RuntimeError(
          f'An unexpected parse tree node of type {type(the_type)} is the '
          f'type for the symbol {name} in the scope {symbols.path}')

  # This scan() callback binds type definitions within the type scope.
  def bind_type_declarations(ast: pascal_parser.TypeDeclarationPart,
                             state: DescentState, _):
    types, symbols = state
    for declaration in ast.types:
      name, the_type = declaration.text, declaration.the_type
      types[name] = get_type(name, types, symbols, the_type)

  # This scan() callback adds variable declarations to the symbol table.
  def bind_variable_declarations(ast: pascal_parser.VariableDeclarationPart,
                                 state: DescentState, _):
    types, symbols = state
    for declaration in ast.declarations:
      name, the_type = declaration.text, declaration.the_type
      symbols[name] = mupas_scopes.VariableSymbol(
          definition=declaration,
          typeinfo=get_type(name, types, symbols, the_type))

  # This scan() callback adds subroutine parameters to the symbol table.
  def bind_subroutine_parameters(ast: pascal_parser.FormalParameterList,
                                 state: DescentState, _):
    types, symbols = state
    for declaration in ast.parameters:
      name, the_type = declaration.text, declaration.the_type
      typeinfo = get_type(name, types, symbols, the_type)
      if declaration.by_reference:
        symbols[name] = mupas_scopes.ReferenceSymbol(
            definition=declaration, typeinfo=typeinfo)
      else:
        symbols[name] = mupas_scopes.VariableSymbol(
            definition=declaration, typeinfo=typeinfo)

  # This scan() callback adds subroutine definitions to the symbol table.
  def bind_subroutine_definitions(
      ast: pascal_parser.ProcedureAndFunctionDefinitionPart,
      state: DescentState, todos: DescentTodos):
    types, symbols = state
    for subroutine in ast.subroutines:
      name = subroutine.heading.text
      # Collecting subroutine parameters in this scope gives us bindings for
      # enumerated type symbols in this scope as well.
      parameters: list[mupas_types.SubroutineParameter] = []
      if subroutine.heading.parameters is not None:
        for declaration in subroutine.heading.parameters.parameters:
          parameters.append(mupas_types.SubroutineParameter(
              name=declaration.text, reference=declaration.by_reference,
              typeinfo=get_type(
                  declaration.text, types, symbols, declaration.the_type)))
      # Add subroutine bindings in this scope.
      if isinstance(subroutine, pascal_parser.ProcedureDefinition):
        symbols[name] = mupas_scopes.SubroutineSymbol(
            definition=subroutine,
            typeinfo=mupas_types.Procedure(parameters=parameters))
      elif isinstance(subroutine, pascal_parser.FunctionDefinition):
        symbols[name] = mupas_scopes.SubroutineSymbol(
            definition=subroutine, typeinfo=mupas_types.Function(
                parameters=parameters, return_typeinfo=get_type(
                    name, types, symbols, subroutine.heading.result_type)))
      # Create nested scopes for this subroutine and schedule processing of
      # those scopes.
      nested_types = TypeScope(name, parent=types)
      nested_symbols = SymbolScope(name, parent=symbols)
      nested_state = (nested_types, nested_symbols)
      # Append in reverse order since the scan() queue is a LIFO.
      todos.append((descend_into_block, subroutine.body.block, nested_state))
      if subroutine.heading.parameters is not None:
        todos.append((bind_subroutine_parameters,
                      subroutine.heading.parameters, nested_state))

  # This scan() callback processes muPas blocks, dispatching handling of those
  # blocks to the specialised handlers defined above.
  def descend_into_block(
      ast: pascal_parser.Block,
      state: DescentState, todos: DescentTodos):
    types, symbols = state
    # Append in reverse order since the scan() queue is a LIFO.
    if ast.procedure_and_function_definition_part is not None:
      todos.append((bind_subroutine_definitions,
                    ast.procedure_and_function_definition_part, state))
    if ast.variable_declaration_part is not None:
      todos.append((bind_variable_declarations,
                    ast.variable_declaration_part, state))
    if ast.type_declaration_part is not None:
      todos.append((bind_type_declarations,
                    ast.type_declaration_part, state))
    if ast.constant_declaration_part is not None:
      todos.append((bind_constant_declarations,
                    ast.constant_declaration_part, state))

  # At last, analyse this program!
  root_types = TypeScope(name=ast.text)
  root_symbols = SymbolScope(name=ast.text)
  root_state = (root_types, root_symbols)
  if extras is not None:
    for name, symbol in extras.items(): root_symbols[name] = symbol
  mupas_descent.scan(descend_into_block, ast.block, root_state)

  return root_symbols


def get_call_graph(
    ast: pascal_parser.Program,
    symbols: mupas_scopes.SymbolScope,
) -> Mapping[str, Collection[str]]:
  """Construct a static call graph for a muPas program.

  Args:
    ast: A parse tree --- must be a Program node.
    symbols: A symbol table (tree) derived from ast by the get_symbols function.

  Returns:
    A mapping from "scope paths" (strings that uniquely identify a subroutine or
    the main program body) to the scope paths of subroutines called from those
    scopes. (A scope path resembles a Unix directory path, so for a program
    called Foo with a function called Baz defined in the top-level procedure
    called Bar, the scope path for Baz would be /Foo/Bar/Baz.)

    Each entry in the returned mapping corresponds to the set of all directed
    call graph edges emanating from the subroutine (or main program body) named
    by the entry's key. Subroutines that are never called anywhere within a
    muPas program (or whose callers are never called anywhere, or whose callers'
    callers etc.) do not appear in the mapping.
  """
  # Convenient type abbreviations.
  SymbolScope = mupas_scopes.SymbolScope
  DescentTodos = list[tuple[mupas_descent.Callback[None, SymbolScope],
                            pascal_parser.AstNode, SymbolScope]]

  # Check whether we have the correct symbol table for this program.
  if ast.text != symbols.name: raise ValueError(
      f"The {symbols.name} symbol table doesn't seem to match the program "
      f'{ast.text}.')

  # We'll be scanning the subroutines (or, initially, program) accumulated in
  # to_process in order to build out the call graph.
  to_process: list[tuple[str, pascal_parser.AstNode]] = [(symbols.path, ast)]
  call_graph: dict[str, set[str]] = {}

  # A rather heavy-handed function for descending into the statements of a
  # subroutine or the main program code. When it encounters a call to another
  # subroutine, it updates the call graph to note who is calling whom.
  def descend_into_statements(
      ast: pascal_parser.AstNode,
      symbols: SymbolScope, todos: DescentTodos):

    # First, note anything here that looks like a subroutine call.
    if isinstance(ast, pascal_parser.BindingReferenceOrCall):
      if isinstance(symbols[ast.binding], mupas_scopes.ExtensionSymbol):
        pass  # An extension; nothing for us to do. The generator deals with it.
      elif isinstance(symbols[ast.binding].typeinfo, mupas_types.Subroutine):
        called_path = symbols.itempath(ast.binding)
        call_graph[symbols.path].add(called_path)
        to_process.append((called_path, symbols[ast.binding].definition.body))
      # TODO: Place other symbols with blocks here if you extend the language.

    # Recursing into the program in a way that avoids mistaking function return
    # value assignment for a procedure call.
    if isinstance(ast, pascal_parser.StatementAssignment):
      todos.append((descend_into_statements, ast.value, symbols))
    else:
      todos.extend((descend_into_statements, kid, symbols)
                   for kid in reversed(mupas_descent.children(ast)))

  # Iterate until we run out of to_process contents.
  while to_process:
    path, ast_with_block = to_process.pop(0)
    if path not in call_graph:  # Skip if we've already processed this path.
      call_graph[path] = set()  # Get ready to build this call graph entry.
      scope = symbols.get_scope(path)  # Get scope for this path.
      statements = ast_with_block.block.statement_part
      mupas_descent.scan(descend_into_statements, statements, scope)

  return call_graph


def get_reverse_transitive_call_graph(
    call_graph: Mapping[str, Collection[str]]
) -> Mapping[str, Collection[str]]:
  """Invert a call graph as computed by `get_call_graph`; add transitive edges.

  Args:
    call_graph: A call graph as computed by `get_call_graph`.

  Returns:
    A mapping from "scope paths" (strings that uniquely identify a subroutine or
    the main program body) to the scope paths of subroutines (or the main
    program body) that call the code identified by the key... or that call a
    subroutine which calls a subroutine which ... etc. calls the code
    identified by the key.

    In general, the code identified by a key in this mapping should not reuse a
    resource that the code named by values indexed under that key uses.
  """
  # First, invert the call graph. We start with the comprehension so that the
  # program (which is not "called" by anyone, but still runs) still gets an
  # entry in the result, which also allows the second part of this function
  # to work.
  rev_call_graph: dict[str, set[str]] = {k: set() for k in call_graph}
  for caller, callees in call_graph.items():
    for callee in callees:
      rev_call_graph.setdefault(callee, set()).add(caller)

  # Now extend the sets of callees transitively.
  get_sets_size = lambda: sum(len(v) for v in rev_call_graph.values())
  sets_size = get_sets_size()
  while True:
    for callees in rev_call_graph.values():
      for callee in list(callees):  # Copy list since we might modify it.
        callees |= rev_call_graph[callee]
    if sets_size == (new_sets_size := get_sets_size()): break
    sets_size = new_sets_size

  return rev_call_graph


def add_static_resources(
    symbols: mupas_scopes.SymbolScope,
    rev_call_graph: Mapping[str, Collection[str]],
    allocator: mupas_static.Allocator):
  """Allocate static global storage resources to symbols in a symbol table.

  This muPas compiler makes use of static global storage resources (e.g.
  variables for 4050 BASIC) for storing the contents of string and array
  constants and variables. In other words, these items are not saved in dynamic
  storage like a heap or a stack, which means that we can't declare string or
  array variables in subroutines that participate in recursion.

  This routine uses the supplied static resource Allocator to choose the
  resources that will be used to store those contents, modifying the entries in
  the symbol table to save those choices.

  Finally, for speed, every variable at program level will be allocated by the
  Allocator (which is faster in 4050 BASIC).

  (All the strategy defined in this docstring is obviously fairly specific to
  the 4050 BASIC target. It should probably be factored out of this module and
  placed somewhere more architecture-specific.)

  Args:
    symbols: A nested symbol table as returned by get_symbols. This table will
        be modified to assign storage resources to string and array variables
        and constants.
    rev_call_graph: A reverse transitive call graph as computed by
        get_reverse_transitive_call_graph.
    allocator: A static resource Allocator for selecting storage resources.

  Raises:
    RuntimeError: A subroutine that could participate in recursion declares a
        string or array variable.
  """
  # Helper: is a Symbol a string constant?
  def is_string_constant(symbol: mupas_scopes.Symbol) -> bool:
    return isinstance(symbol, mupas_scopes.ConstantStringSymbol)

  # First, assign storage resources for constant strings.
  for path in rev_call_graph:
    for v in symbols.get_scope(path).bindings.values():
      if is_string_constant(v): v.storage = allocator.allocate(v.typeinfo)

  # Helper: is a Symbol a string or array variable?
  def is_string_or_array_variable(symbol: mupas_scopes.Symbol) -> bool:
    return (isinstance(symbol, mupas_scopes.VariableSymbol) and
            isinstance(symbol.typeinfo, (
                mupas_types.String, mupas_types.Array1d, mupas_types.Array2d)))

  # Next, identify subroutines that make recursive calls. These subroutines are
  # not allowed to have their own string or array variables, since we don't
  # store those in any kind of dynamic memory (e.g. a stack or a heap). So,
  # make sure they don't have those things.
  recursives = set(k for k, v in rev_call_graph.items() if k in v)
  for r in recursives:
    sub_symbols = symbols.get_scope(r)
    bad_bindings = sorted(k for k, v in sub_symbols.bindings.items()
                          if is_string_or_array_variable(v))
    if bad_bindings: raise RuntimeError(
        f"Subroutine {r} could call itself recursively, which means it can't "
        f'declare array/string variables like {", ".join(bad_bindings)}.')

  # The recursive subroutines are our first "retired" subroutines --- that is,
  # we don't have to think about them anymore.
  retired = recursives

  # Now assign a storage resource to all variables at the program ("top") level.
  # This is for speed --- we assume that accessing the stack is slower.
  for v in symbols.bindings.values():
    if isinstance(v, mupas_scopes.VariableSymbol):
      v.storage = allocator.allocate(v.typeinfo)
  retired.add(symbols.path)  # This retires the program level.

  # Helper: identify clusters of scope paths in the reverse transitive call
  # graph (RTCG). A cluster is a set of subroutines (identified by scope paths)
  # that are connected in the RTCG, even indirectly. Additionally, this helper
  # will only consider the RTCG subgraph whose nodes are listed in paths and
  # whose edges connect those nodes.
  def rev_call_graph_clusters(paths: set[str]) -> list[set[str]]:
    # This algorithm requires rev_call_graph to be transitive.
    clusters: list[set[str]] = []
    for k, v in rev_call_graph.items():
      if subset := set.union({k}, v) & paths:  # Join key and value in 1 entry.
        for c in clusters:
          if c & subset:  # If this subset overlaps a cluster...
            c |= subset   # ...add the subset to the cluster.
            break
        else:
          clusters.append(subset)  # Otherwise start a new cluster.
    return clusters

  # Helper: identify scope paths in a set of scope paths that have no callers
  # listed in the reverse call graph, not counting any elements in retired.
  def rev_call_graph_roots(paths: set[str]) -> set[str]:
    return {p for p in paths if not (set(rev_call_graph[p]) - retired)}

  # Helper: recursion for assigning storage resources to string and array
  # variables in the call stack. The recursion works by considering clusters:
  # subsets of subroutines inside the program.
  #
  # Consider the call graph --- a DAG since we're restricting to subroutines
  # that don't participate in recursion. The first cluster is the entire DAG.
  # We assign storage resources for subroutines in the cluster that aren't
  # called by any other subroutine in the cluster --- so-called "roots". 
  #
  # We then remove those subroutines from the DAG and identify connected
  # components in what remains: the recursion analyses those sub-clusters next.
  # Once the recursive calls complete, we free the storage resources used by
  # the roots and return.
  def rec(cluster: set[str]):
    if cluster:
      # Compute roots of this cluster and mark those roots as retired.
      assert (roots := rev_call_graph_roots(cluster))
      retired.update(roots)

      # For each root, allocate symbols to string and array variables in
      # that root.
      resources = []
      for root in roots:
        for v in symbols.get_scope(root).bindings.values():
          if is_string_or_array_variable(v):
            resources.append(resource := allocator.allocate(v.typeinfo))
            v.storage = resource

      # Recurse into subclusters of this cluster.
      for subc in rev_call_graph_clusters(cluster - roots): rec(subc)

      # Release all allocated symbols for string and array variables.
      for resource in resources: allocator.release(resource)

  # Start the recursion on clusters of all non-retired scope paths in the
  # program: allocate global storage resources for all string and array
  # variables in non-retired subroutines.
  for cluster in rev_call_graph_clusters(set(rev_call_graph) - retired):
    rec(cluster)


def add_stack_resources(
    symbols: mupas_scopes.SymbolScope,
    allocator: mupas_stack.FrameAllocator):
  r"""Allocate stack resources to symbols in a symbol table.

  The muPas compiler makes use of resources on a stack for scalar numeric
  values local to a subroutine: this includes non-reference parameters, local
  variables, and (for functions) return values. To be more precise, the
  compiler makes use of a stack *abstraction* provided by the mupas_stack
  module: the generated code may do something different.

  A(n abstracted) muPas stack frame takes the following form:

     [ Return value (if a function)  ]   <Positive, incrementing indices>
     [ Return address                ]
     [ Old frame pointer             ]     |  Direction of stack growth
     [ Pointer to enclosing scope    ]     |
     [ Subroutine scalar parameter 1 ]   \ | /
     [ Subroutine scalar parameter 2 ]    \|/
     [ ...                           ]     v
     [ Local scalar variable 1       ]
     [ Local scalar variable 2       ]
     [ ...                           ]

  This routine uses the supplied stack resource allocator to choose the stack
  resources that will be allocated to local scalar numeric values. The
  allocator and the rest of the compiler can count on the first three allocated
  positions (first *four* for functions) to be chosen as shown in the diagram
  above --- or, put differently, it can count on a procedure call to have a
  CodePointer at the first position (for the return address), and on a function
  call to have a CodePointer at the second position.

  The allocator should probably remain agnostic to the purpose and arrangement
  of all other positions in the (abstracted) stack frame besides the ones
  mentioned above.

  Args:
    symbols: A nested symbol table as returned by get_symbols AND WHOSE STATIC
        GLOBAL STORAGE RESOURCES HAVE ALREADY BEEN ALLOCATED BY
        add_static_resources. This table will be modified to assign
        storage resources to subroutine parameters and local variables.
    allocator: For allocating stack frames.
  """
  # Helper: is a Symbol a subroutine?
  def is_subroutine(symbol: mupas_scopes.Symbol) -> bool:
    return isinstance(symbol, mupas_scopes.SubroutineSymbol)

  # Helper: is a Symbol a scalar number variable with no storage allocated yet?
  def is_storageless_scalar_number_var(symbol: mupas_scopes.Symbol) -> bool:
    return (isinstance(symbol, mupas_scopes.VariableSymbol) and
            isinstance(symbol.typeinfo, mupas_types.ScalarNumber) and
            symbol.storage is None)

  # Helper: for a subroutine, allocate a stack frame and add storage to
  # variables and/including parameters and return values. (So the name doesn't
  # necessarily say it all. :-) This is where the abstracted stack frame
  # structure depicted above is built.
  def allocate_stack_frame_and_add_storage_to_variables(
      typeinfo: mupas_types.Subroutine,
      symbols: mupas_scopes.SymbolScope,
  ) -> mupas_stack.Frame:
    # Assemble list of resources for the stack frame.
    frame_types: list[mupas_types.Type] = []
    if isinstance(typeinfo, mupas_types.Function):  # First: the return type, if
      frame_types.append(typeinfo.return_typeinfo)  # this is a function.

    frame_types.append(mupas_types.CodePointer())  # Next, the return address.
    frame_types.append(mupas_types.DataPointer())  # Then: old frame pointer.
    frame_types.append(mupas_types.DataPointer())  # Enclosing scope pointer.

    for parameter in typeinfo.parameters:  # Next, subroutine parameters.
      if not parameter.reference: frame_types.append(parameter.typeinfo)
    parameter_names = {p.name for p in typeinfo.parameters}  # For later.

    for k, v in symbols.bindings.items():  # Finally, local variables.
      if not k in parameter_names:
        if is_storageless_scalar_number_var(v):
          frame_types.append(v.typeinfo)  # type: ignore # mypy bug

    # Now allocate the stack frame!
    frame = allocator.allocate(frame_types)

    # With the stack frame to hand, we can now repeat much of the iteration
    # above to add the stack storage information to variables.
    frame_index = 3 + isinstance(typeinfo, mupas_types.Function)
    for parameter in typeinfo.parameters:  # Subroutine parameters.
      if not parameter.reference:
        symbol = symbols.bindings[parameter.name]
        assert isinstance(symbol, mupas_scopes.VariableSymbol)  # for mypy
        symbol.storage = frame[frame_index]
        frame_index += 1
    for k, v in symbols.bindings.items():  # Local variables.
      if not k in parameter_names:
        if is_storageless_scalar_number_var(v):
          v.storage = frame[frame_index]  # type: ignore # mypy bug
          frame_index += 1

    return frame

  # Helper: recurse into subroutines and nested subroutines, allocating stack
  # resources for local scalar numeric values within.
  def rec(symbols: mupas_scopes.SymbolScope):
    for k, v in symbols.bindings.items():
      if is_subroutine(v):
        assert isinstance(v, mupas_scopes.SubroutineSymbol)  # mypy bug
        v.frame = allocate_stack_frame_and_add_storage_to_variables(
            v.typeinfo, symbols.children[k])
        rec(symbols.children[k])

  rec(symbols)
