"""muPas code generation for the Tektronix 4050-series BASIC target.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

The code generator generates Tektronix 4050-series BASIC code from fragments
that it assembles together as it recursively descends into the parse tree. To
accomplish this, it must also know the storage allocation decisions made by the
functions in the `mupas_analyses` module. This code generator does not make
meaningful use of the traversal aids in the `mupas_descent` module, though it 
could be written to do that if we really wanted.

This module is organised into four sections: (1) functions that generate code
for higher-level program structures like subroutines, (2) functions that
generate code for statements, (3) functions that generate code for expressions,
and (4) general utilities.
"""

import dataclasses
import itertools
import warnings

import mupas_descent
import mupas_scopes
import mupas_types
import pascal_parser
import t4050_compiled
import t4050_extensions
import t4050_resources
import t4050_types

from typing import Collection, Mapping, Optional, Sequence


# Some constants for exp_expression_binary.
_ARRAY_TYPES = (mupas_types.Array1d, mupas_types.Array2d)
_EASY_BINARY_OPS = {
    pascal_parser.BinaryOp.MULTIPLY: '*',
    pascal_parser.BinaryOp.DIVIDE_TO_REAL: '/',
    pascal_parser.BinaryOp.LOGICAL_AND: ' AND ',
    pascal_parser.BinaryOp.ADD: '+',
    pascal_parser.BinaryOp.SUBTRACT: '-',
    pascal_parser.BinaryOp.LOGICAL_OR: ' OR ',
    pascal_parser.BinaryOp.COMPARE_EQ: '=',
    pascal_parser.BinaryOp.COMPARE_LT: '<',
    pascal_parser.BinaryOp.COMPARE_GT: '>',
    pascal_parser.BinaryOp.COMPARE_LE: '<=',
    pascal_parser.BinaryOp.COMPARE_GE: '>=',
    pascal_parser.BinaryOp.COMPARE_NE: '<>',
}


#########################
### PROGRAM STRUCTURE ###
#########################


@dataclasses.dataclass
class Label:
  """Bookkeeping for muPas labels.

  Attributes:
    name: Mangled name for a label, used in the 4050 BASIC "assembly".
    defined: Whether a declared label was actually defined in the code.
    used: Whether a declared label was ever used by the code.
  """
  name: str
  defined: bool = False
  used: bool = False


class Labels(dict[int, Label]):
  """muPas label registry, 4050 BASIC label generator.

  A bit of a junk drawer object for all things label. As a dict, Labels maps
  the non-negative integers that Pascal uses for GOTO labels to the labels
  used in our 4050 BASIC "assembly" code. There is also helper code for
  generating unique 4050 BASIC labels.
  """
  _serial: int

  def __init__(self, *args, **kwargs):
    super().__init__(*args, **kwargs)
    self._serial = 0

  def generate(
      self,
      kind: str,
      symbols: mupas_scopes.SymbolScopeProtocol,
      serial: Optional[int] = None,
  ) -> str:
    """Create a unique 4050 BASIC label.

    Achieves uniqueness by combining information from the arguments with an
    incrementing serial number kept within this Labels object.

    Args:
      kind: Arbitrary string useful for distinguishing this label within the
          "assembly" code --- for example, a label used by generated code
          for an IF statement might use 'IfElse' as a label for the ELSE
          clause.
      symbols: Symbol table for the scope where this label is used --- the name
          of the scope will also be incorporated into the label.
      serial: Specify a value here to override the serial number that this
          method generates for the label.

    Returns:
      A string usable as a 4050 BASIC label.
    """
    if serial is None:
      serial = self._serial
      self._serial += 1
    label = f'_{kind}{symbols.path.replace("/", "_")}_{serial:03}'
    return label


@dataclasses.dataclass
class ProgramOptions:
  """Special options for program generation.

  Attributes:
    origin: Line number for the compiled program's first BASIC statement, not
        counting any statement generated for the `line1_jump` option.
    line_increment: Spacing between successive line numbers in the compiled
        program.
    line1_jump: If True, the program will begin with a GO TO from BASIC line 1
        to the origin specified by the `origin` option.
  """
  origin: int = 100
  line_increment: int = 10
  line1_jump: bool = True


def program(
    ast: pascal_parser.Program,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    static_allocator: t4050_resources.StaticAllocator,
    frame_allocator: t4050_resources.FrameAllocator,
    stack_allocator: t4050_resources.StackAllocator,
    rev_call_graph: Mapping[str, Collection[str]],
    options: Optional[ProgramOptions] = None,
) -> Sequence[str]:
  """Generate: for Program AST nodes.

  Ordinarily the AST-node-to-code-fragment functions in this module aren't
  extensively commented, but as this is the one most likely to be invoked by
  a caller external to this module, we'll make an exception.

  Args:
    ast: Abstract syntax tree for a muPas program.
    symbols: Symbol table for the program.
    frame: A muPas abstracted stack frame for the program. This frame can take
        the form of a stack frame for a procedure with no arguments: that is,
        it should contain (in order) space for a `CodePointer` followed by
        slots for two `DataPointer`s. The stack frame, which will not actually
        consume any space on its own in the final program, should have been
        allocated from `frame_allocator`.
    quoted_constants: Quoted constants for the program.
    static_allocator: The static resources allocator used to allocate the
        static resources in `symbols`.
    frame_allocator: The frame allocator used to allocate stack frames used
        by stack-based resources in `symbols`.
    stack_allocator: The stack allocator used to allocate the stack used by
        stack-based resources in `symbols`.
    rev_call_graph: Reverse transitive call graph for the program as derived by
        `mupas_analyses.get_reverse_transitive_call_graph`.
    options: Special options for program generation.

  Returns:
    Tektronix 4050 BASIC "assembly" code suitable for use with the "assembler"
    in the `t4050_assembler` module.
  """
  # We will already have checked that these components are not used in
  # mupas_analyses.check_parse_tree, so we just assert here.
  assert ast.parameters is None
  assert ast.uses_clause is None

  # Use default program options if none are supplied.
  if options is None: options = ProgramOptions()

  # Code start: initialise the BASIC interpreter.
  code: list[str] = [f'      INC {options.line_increment}']
  program_origin_label = f'_Program{symbols.path.replace("/", "_")}'
  if options.line1_jump:
    code.extend(['      ORG 1', f'      GO TO |{program_origin_label}|'])
  code.extend([f'      ORG {options.origin}',
               f'{program_origin_label}:',
               '      INIT'])

  # First order of business: define string constants by sticking them into
  # string variables. Even if the scope of the constants is local, they are
  # defined as globals in BASIC -- and then (hopefully!) never changed. So
  # we recurse into the symbol table to extract and define all string constants
  # at top level. Note that we're generating code for both setup and teardown
  # of string constants.
  const_string_init: list[str] = []    # Dimensioning vars for constants.
  const_string_define: list[str] = []  # Assigning values to the constants.
  const_string_delete: list[str] = []  # Deleting variables for constants.
  def const_string_code(symbols: mupas_scopes.SymbolScopeProtocol):
    #nonlocal const_string_init, const_string_define, const_string_delete
    # Define string constants in the current scope.
    for symbol in symbols.bindings.values():
      if isinstance(symbol, mupas_scopes.ConstantStringSymbol):
        if ((storage := symbol.storage) is None or
            not isinstance(storage, t4050_resources.StringVariable)):
          raise _InternalError  # Was allocation skipped?
        # Collect value of the string variable.
        value = quoted_constants[symbol.index]
        literal = '"' + value.replace('"', '""') + '"'  # 4050 BASIC escaping.
        # Collect code for dimensioning, defining, and deleting.
        const_string_init.extend(
            static_allocator.init_info(storage).code(symbol.typeinfo))
        const_string_define.append(
            f'      {storage.variable_name}={literal}')
        const_string_delete.extend(
            static_allocator.delete_info(storage).code())
    # Recurse into the child scopes
    for child_scope in symbols.children.values(): const_string_code(child_scope)

  const_string_code(symbols)
  code.extend(const_string_init)
  code.extend(const_string_define)

  # Second order of business: set up the stack and the stack frame for the
  # main program.
  code.extend(stack_allocator.init_info(frame_allocator).code)
  # Advance the stack pointer to allocate stack space for any stack-using local
  # variables in the main program.
  num_variables_on_stack = frame.num_elements - frame.locals_position
  assert num_variables_on_stack >= 0
  if num_variables_on_stack > 0:
    sp = frame.allocator.stack_pointer_resource.variable_name
    code.append(f'      {sp}={sp}+{num_variables_on_stack}')

  # Compile this program's code block.
  code.extend(block(ast.block, symbols, frame,
                    quoted_constants, static_allocator, rev_call_graph))

  # Finish off the code by deleting string constants, deleting the stack, then
  # issuing an END statement that terminates the program.
  code.extend(const_string_delete)
  code.extend(stack_allocator.delete_info(frame_allocator).code)
  code.append('      END')
  # Thanks for compiling with us today!
  return code


def block(
    ast: pascal_parser.Block,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    static_allocator: t4050_resources.StaticAllocator,
    rev_call_graph: Mapping[str, Collection[str]],
) -> list[str]:
  """Generate: for Block AST nodes."""
  # Label declarations, if present.
  labels = Labels()
  if ast.label_declaration_part is not None:
    for label_str in ast.label_declaration_part.labels:
      label_int = int(label_str)
      labels[label_int] = Label(name=labels.generate('Label', symbols))

  # Constants and types for this block will already have been understood by
  # code in mupas_analyses.

  code: list[str] = []  # Code accumulates here.

  # Generate the setup code for the block: mainly variable allocation for
  # native 4050 BASIC variables used by this block. Variables on the stack will
  # have already been allocated by the stack setup procedure.
  if ast.variable_declaration_part is not None:
    # Accumulate deletions individually --- we'll try to combine them later
    # when optimising assembly language code.
    for name_and_type in ast.variable_declaration_part.declarations:
      symbol = symbols[name_and_type.text]
      assert isinstance(symbol, mupas_scopes.VariableSymbol)
      match symbol.storage:
        case (t4050_resources.StringVariable() |
              t4050_resources.NumericVariable()):
          typeinfo = symbol.typeinfo
          code.extend(static_allocator.init_info(symbol.storage).code(typeinfo))
        case t4050_resources.StackValue():
          continue
        case _:
          raise _InternalError  # There should be no other type of storage.

  # Compile all of the statements in this block.
  for statement in ast.statement_part.statements.statements:
    code.extend(sta_statement(
        statement, symbols, frame, quoted_constants, labels).code)

  # Generate the teardown code for the block: mainly variable deallocation
  # for native 4050 BASIC variables used by this block. Variables on the stack
  # will be deallocated by the caller's stack teardown procedure.
  code.append(f'_Exit{symbols.path.replace("/", "_")}:')
  if ast.variable_declaration_part is not None:
    # Accumulate deletions individually --- we'll try to combine them later
    # when optimising assembly language code.
    for name_and_type in ast.variable_declaration_part.declarations:
      symbol = symbols[name_and_type.text]
      assert isinstance(symbol, mupas_scopes.VariableSymbol)
      match symbol.storage:
        case (t4050_resources.StringVariable() |
              t4050_resources.NumericVariable()):
          code.extend(static_allocator.delete_info(symbol.storage).code())
        case t4050_resources.StackValue():
          continue
        case _:
          raise _InternalError  # There should be no other type of storage.

  # Finally, generate subroutines defined in this block, if any. Note that
  # if there are, we'll need to jump past the subroutines in order to get to
  # the end of the block.
  if ast.procedure_and_function_definition_part is not None:
    procedure_and_function_definitions: list[str] = []

    for subroutine in ast.procedure_and_function_definition_part.subroutines:
      # Obtain the symbol table entry for this subroutine.
      subroutine_definitions = (
          pascal_parser.FunctionDefinition, pascal_parser.ProcedureDefinition)
      if not isinstance(subroutine, subroutine_definitions):
        raise _InternalError  # There should be no other kind.
      name = subroutine.heading.text
      symbol = symbols[name]
      if not isinstance(symbol, mupas_scopes.SubroutineSymbol):
        raise _InternalError  # This would also be deeply strange.

      # Skip this subroutine if it isn't used anywhere.
      if symbols.itempath(name) not in rev_call_graph: continue

      # Obtain the stack frame bookkeeping data for this subroutine. Our
      # recursive processing will mutate this frame, but in all cases we should
      # be undoing the mutations that get made.
      subroutine_frame = symbol.frame
      assert isinstance(subroutine_frame, t4050_resources.Frame)
      if subroutine_frame is None:
        raise _InternalError  # And this would mean allocation was skipped.

      # Obtain the symbol table for this subroutine.
      subroutine_symbols = symbols.children[name]

      # Generate code for the subroutine at last.
      procedure_and_function_definitions.extend(subroutine_definition(
          subroutine, subroutine_symbols, subroutine_frame,
          quoted_constants, static_allocator, rev_call_graph))

    # If we defined any procedures or functions, then include them in the
    # generated code. We'll need to jump around these definitions so that
    # block code defined above can jump past them to the end of the generated
    # code for this block (and from there to any code that gets appended).
    if procedure_and_function_definitions:
      block_end_label = labels.generate('BlockEnd', symbols)
      code.append(f'      GO TO |{block_end_label}|')
      code.extend(procedure_and_function_definitions)
      code.append(f'{block_end_label}:')

  # Before we leave: see if labels have been defined and used.
  for label, info in labels.items():
    if not (info.defined or info.used): warnings.warn(
        f'Label {label} is declared but not defined or used in {symbols.path}')
    if info.defined and not info.used: warnings.warn(
        f'Label {label} is defined and not used in {symbols.path}')
    if info.used and not info.defined: raise RuntimeError(
        f'Label {label} is used but not defined in {symbols.path}')

  return code


def subroutine_definition(
    ast: pascal_parser.SubroutineDefinition,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    static_allocator: t4050_resources.StaticAllocator,
    rev_call_graph: Mapping[str, Collection[str]],
) -> list[str]:
  """Generate: for ProcedureDefinition and FunctionDefinition AST nodes."""
  # Make certain that this is a subroutine that we can compile.
  assert isinstance(ast, (
      pascal_parser.FunctionDefinition, pascal_parser.ProcedureDefinition))
  assert isinstance(ast.body, pascal_parser.SubroutineBodyBlock)

  # Kick off the code for this subroutine by emitting a label and a comment
  # for the reader's sake.
  code: list[str] = []
  subroutine_path = symbols.path.replace('/', '_')
  code.append(f'      REM {symbols.path}')
  code.append(f'_SubEnter{subroutine_path}:')  # Label for GOSUB

  # Now all we have to do is compile the subroutine's body block. A lot of the
  # heavy-lifting has happened elsewhere: information about where parameters
  # and variables go have been computed elsewhere and saved in `symbols` and in
  # `frame`; stack preparation is all the responsibility of the caller.
  code.extend(block(ast.body.block, symbols, frame,
                    quoted_constants, static_allocator, rev_call_graph))

  # Return to the caller; subroutine is now compiled!
  code.append('      RET')
  return code


##################
### STATEMENTS ###
##################


def sta_statement(
    ast: pascal_parser.Statement,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for Statement AST nodes.

  Ordinarily the AST-node-to-code-fragment functions in this module aren't
  extensively commented, but as this one has an argument structure that's
  (nearly) replicated by most of the code generating functions for statements,
  it seems useful to document those arguments here.

  Args:
    ast: Abstract syntax tree for a muPas statement.
    symbols: Symbol table for the statement's scope.
    frame: A muPas abstracted stack frame for the statement's scope.
    quoted_constants: Quoted constants for the program.
    labels: Label generator and repository for the statement's scope.

  Returns:
    A compiled code fragment for the statement.
  """
  # Generate code that will execute the statement.
  match ast:
    case pascal_parser.StatementAssignment():
      compiled = sta_assignment(ast, symbols, frame, quoted_constants, labels)
    case pascal_parser.StatementProcedure():
      compiled = sta_procedure(ast, symbols, frame, quoted_constants, labels)
    case pascal_parser.StatementGoto():
      compiled = sta_goto(ast, symbols, frame, quoted_constants, labels)
    case pascal_parser.StatementIf():
      compiled = sta_if(ast, symbols, frame, quoted_constants, labels)
    case pascal_parser.StatementCase():
      compiled = sta_case(ast, symbols, frame, quoted_constants, labels)
    case pascal_parser.StatementFor():
      compiled = sta_for(ast, symbols, frame, quoted_constants, labels)
    case pascal_parser.StatementRepeat():
      compiled = sta_repeat(ast, symbols, frame, quoted_constants, labels)
    case pascal_parser.StatementWhile():
      compiled = sta_while(ast, symbols, frame, quoted_constants, labels)
    case pascal_parser.StatementCompound():
      compiled = t4050_compiled.chain_statements([
          sta_statement(s, symbols, frame, quoted_constants, labels)
          for s in ast.statements])
    case pascal_parser.LabelAndStatement():
      compiled = sta_label_and_statement(
          ast, symbols, frame, quoted_constants, labels)
    case _:
      raise _UnexpectedParseTreeNode

  # Clean up any extra use of the stack.
  frame.release_n(compiled.stack_growth)  # All works fine if growth is 0.
  delete_info = frame.delete_info(frame[-1])  # Frame entry doesn't matter.
  return t4050_compiled.chain_statements([
      t4050_compiled.Statement(code=compiled.code),
      t4050_compiled.Statement(
          code=delete_info.for_n_places(compiled.stack_growth))])


def sta_assignment(
    ast: pascal_parser.StatementAssignment,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for StatementAssignment AST nodes."""
  symbol = symbols[ast.destination.binding]
  match symbol:
    case mupas_scopes.VariableSymbol():
      # In 4050 BASIC, the code for accessing a scalar stored value for
      # assignment is the same as the code for accessing it for reading. So
      # we can simply reuse the code for evaluating a variable.
      compiled_lhs = exp_variable_access(
          symbol, ast.destination, symbols, frame, quoted_constants)
    case mupas_scopes.SubroutineSymbol():
      # For setting a function return value, we concoct a
      # t4050_compiled.Expression whose `access` member yields the return value
      # stack location. Note how this return value can only be on the left hand
      # side. Note also how we subtract 1 from `hops`: that's because the
      # function's name is bound in the scope that encloses the function's own
      # scope.
      if not isinstance(symbol.typeinfo, mupas_types.Function):
        raise RuntimeError(
            'Attempt to assign a return value for the subroutine '
            f"{symbols.itempath(ast.destination.binding)}, which isn't a "
            'function')
      stack = frame.allocator.stack_resource.variable_name
      fp = frame.allocator.frame_pointer_resource.variable_name
      hops = symbols.itemhops(ast.destination.binding) - 1
      for _ in range(hops): fp = f'{stack}[{fp}-1]'
      compiled_lhs = t4050_compiled.Expression(
          typeinfo=symbol.typeinfo.return_typeinfo,
          access=f'{stack}[{fp}-3]', can_be_lhs=True)
    case _:
      raise _UnexpectedParseTreeNode

  # The value to assign to the stored value is simply an expression.
  compiled_rhs = exp_expression(ast.value, symbols, frame, quoted_constants)
  t4050_types.check_assignment_or_parameter_compatibility(  # Check types, sure.
      compiled_lhs.typeinfo, compiled_rhs.typeinfo)

  # Return the assignment itself.
  return t4050_compiled.chain_statements([
      t4050_compiled.Statement(code=compiled_lhs.compute,
                               stack_growth=compiled_lhs.stack_growth),
      t4050_compiled.Statement(code=compiled_rhs.compute,
                               stack_growth=compiled_rhs.stack_growth),
      t4050_compiled.Statement(code=[
          f'      {compiled_lhs.access}={compiled_rhs.access}'])])


def sta_procedure(
    ast: pascal_parser.StatementProcedure,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for StatementAssignment AST nodes."""
  # Retrieve the subroutine/extension symbol and the parameters of its
  # call/invocation, if any.
  symbol = symbols[ast.call.binding]
  parameters = extract_call_parameters(ast.call.binding, ast.call)

  match symbol:
    # For an ordinary subroutine call.
    case mupas_scopes.SubroutineSymbol():
      # Do some parameter checks (others happen in setup_stack_... below.)
      check_call_parameters_length(ast.call.binding, symbol, parameters)

      # Set up the stack for the function call, call the function, and clean up.
      # If we're calling a function, we pop the return value off the stack.
      code, _ = setup_stack_for_subroutine_and_call(
          ast.call.binding, symbol, parameters, symbols, frame,
          quoted_constants, keep_return_value=False)

    # For a extension-style procedure.
    case (t4050_extensions.ExtensionProcedureSymbol() |
          t4050_extensions.ExtensionFunctionSymbol()):
      # Invoke the extension, which is responsible for its own param. checks.
      code = invoke_extension_as_statement(
          symbol, parameters, symbols, frame, quoted_constants)

    # For Exit: jump to the exit point for the current block.
    case t4050_extensions.ExtensionExitSymbol():
      if parameters: raise RuntimeError(
          'For 4050 BASIC, Exit takes no parameters.')
      code = [f'      GO TO |_Exit{symbols.path.replace("/", "_")}|']

    case _:
      raise RuntimeError(
          f'Attempt to call {ast.call.binding}, which is a {type(symbol)}, '
          "which can't be used for calls with the 4050 BASIC target.")

  return t4050_compiled.Statement(code=code)


def sta_goto(
    ast: pascal_parser.StatementGoto,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for StatementGoto AST nodes."""
  try:
    label = labels[ast.label]
  except KeyError:
    raise RuntimeError(f'There is no label {ast.label} in {symbols.path}')
  label.used = True  # Avert warnings of unused labels.
  return t4050_compiled.Statement(code=[f'      GO TO |{label.name}|'])


def sta_if(
    ast: pascal_parser.StatementIf,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for StatementIf AST nodes."""
  compiled_condition = exp_expression(
      ast.condition, symbols, frame, quoted_constants)
  t4050_types.check_assignment_or_parameter_compatibility(
      mupas_types.Boolean(), compiled_condition.typeinfo)

  def _sta_if_stack_growth() -> t4050_compiled.Statement:
    # A general way to compile IF statements if compiled_condition grows the
    # stack. This implementation is careful to pop the extra stack elements
    # right away in both the consequent and alternative clauses, since a GOTO
    # in either place may mean we may never get the chance if we wait to do it
    # later.

    # Get ready to clean up the stack.
    delete_info = frame.delete_info(frame[-1])  # Frame entry doesn't matter.
    frame.release_n(compiled_condition.stack_growth)  # Bookkeeping cleanup.
    stack_pop_code = delete_info.for_n_places(compiled_condition.stack_growth)

    # Generate the 4050 BASIC IF statement and the beginning of the
    # alternative clause. Note how stack-popping happens right away.
    then_label = labels.generate('Then', symbols)
    out_label = labels.generate('EndIf', symbols)
    statements = [
        t4050_compiled.Statement(code=compiled_condition.compute),  # growth: 0!
        t4050_compiled.Statement(code=[
            f'      IF {compiled_condition.access} THEN |{then_label}|']),
        t4050_compiled.Statement(code=stack_pop_code)]

    # Add further alternative code if it exists, then the GOTO to jump past the
    # consequent.
    if ast.alternative is not None:
      statements.append(sta_statement(
          ast.alternative, symbols, frame, quoted_constants, labels))
    statements.append(
        t4050_compiled.Statement(code=[f'      GO TO |{out_label}|']))

    # Add the consequent (with stack-popping first), and the end label.
    statements.extend([
        t4050_compiled.Statement(code=[f'{then_label}:']),
        t4050_compiled.Statement(code=stack_pop_code),
        sta_statement(ast.consequent, symbols, frame, quoted_constants, labels),
        t4050_compiled.Statement(code=[f'{out_label}:'])])

    return t4050_compiled.chain_statements(statements)

  def _sta_if_no_stack_growth() -> t4050_compiled.Statement:
    # A more streamlined way to compile IF statements if compiled_condition
    # does not grow the stack.
    if ast.alternative is None:
      # For if statements with no "else clause". Generated code skips the "then
      # clause" if the condition is false.
      out_label = labels.generate('EndIf', symbols)
      return t4050_compiled.chain_statements([
          t4050_compiled.Statement(
              code=compiled_condition.compute,
              stack_growth=compiled_condition.stack_growth),
          t4050_compiled.Statement(code=[
              f'      IF NOT {compiled_condition.access} THEN |{out_label}|']),
          sta_statement(
              ast.consequent, symbols, frame, quoted_constants, labels),
          t4050_compiled.Statement(code=[f'{out_label}:'])])
    else:
      # For if statements with both a "then clause" and an "else clause".
      # Generated code 
      compiled_alternative = sta_statement(
          ast.alternative, symbols, frame, quoted_constants, labels)
      then_label = labels.generate('Then', symbols)
      out_label = labels.generate('EndIf', symbols)
      return t4050_compiled.chain_statements([
          t4050_compiled.Statement(
              code=compiled_condition.compute,
              stack_growth=compiled_condition.stack_growth),
          t4050_compiled.Statement(code=[
              f'      IF {compiled_condition.access} THEN |{then_label}|']),
          sta_statement(
              ast.alternative, symbols, frame, quoted_constants, labels),
          t4050_compiled.Statement(code=[f'      GO TO |{out_label}|',
                                         f'{then_label}:']),
          sta_statement(
              ast.consequent, symbols, frame, quoted_constants, labels),
          t4050_compiled.Statement(code=[f'{out_label}:'])])

  if compiled_condition.stack_growth != 0:
    return _sta_if_stack_growth()
  else:
    return _sta_if_no_stack_growth()


def sta_case(
    ast: pascal_parser.StatementCase,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for StatementCase AST nodes."""
  # Start with the selector
  compiled_selector = exp_expression(
      ast.selector, symbols, frame, quoted_constants)
  t4050_types.check_assignment_or_parameter_compatibility(
      mupas_types.Ordinal(), compiled_selector.typeinfo)

  # We may need to clean up the stack if the case selector grows the stack.
  # Happily, "releases" and "deletions" are no-ops if we don't need to do that.
  delete_info = frame.delete_info(frame[-1])  # Frame entry doesn't matter.
  frame.release_n(compiled_selector.stack_growth)  # Bookkeeping cleanup.
  stack_pop_code = delete_info.for_n_places(compiled_selector.stack_growth)

  # Labels we'll need.
  case_labels = [labels.generate(f'Case{i+1}', symbols)
                 for i in range(len(ast.cases))]
  end_label = labels.generate('CaseEnd', symbols)

  # Generate tests and jumps for each case.
  statements = [t4050_compiled.Statement(code=compiled_selector.compute)]
  for ast_case, case_label in zip(ast.cases, case_labels):
    # A case can include multiple matching constants.
    tests = []
    for constant in ast_case.constants:
      constant_value = get_integer_constant(constant, symbols)
      tests.append(f'{compiled_selector.access}={constant_value}')
    # Create the "jump table" IF statement for this case.
    statements.append(t4050_compiled.Statement(
        code=[f'      IF {" OR ".join(tests)} THEN |{case_label}|']))

  # Generate the "otherwise" clause, if present; either way, include a general
  # fall-through past the end of the case statement.
  statements.append(t4050_compiled.Statement(code=stack_pop_code))
  if ast.otherwise is not None: statements.append(sta_statement(
      ast.otherwise, symbols, frame, quoted_constants, labels))
  statements.append(
      t4050_compiled.Statement(code=[f'      GO TO |{end_label}|']))

  # Generate consequents for each case.
  for ast_case, case_label in zip(ast.cases, case_labels):
    statements.append(t4050_compiled.Statement(code=[f'{case_label}:']))
    statements.append(t4050_compiled.Statement(code=stack_pop_code))
    statements.append(sta_statement(
        ast_case.consequent, symbols, frame, quoted_constants, labels))
    statements.append(
        t4050_compiled.Statement(code=[f'      GO TO |{end_label}|']))

  # Actually, we can delete that last GO TO because the very next line is the
  # exit beyond the end of the CASE statement.
  statements.pop()

  # Case statement exit.
  statements.append(t4050_compiled.Statement(code=[f'{end_label}:']))

  return t4050_compiled.chain_statements(statements)


def sta_for(
    ast: pascal_parser.StatementFor,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for StatementFor AST nodes."""
  # Retrieve a binding for the control variable and do some checks.
  symbol = symbols[ast.control_variable]
  hops = symbols.itemhops(ast.control_variable)
  if not isinstance(symbol, mupas_scopes.VariableSymbol): raise RuntimeError(
      'The 4050 BASIC target requires FOR loop control variables to be an '
      f'ordinal variable, not a {type(symbol).__name__}')
  if not isinstance(symbol.typeinfo, mupas_types.Ordinal): raise RuntimeError(
      'The 4050 BASIC target requires FOR loop control variables to have an '
      f'ordinal type, not {symbol.typeinfo}')
  # The BASIC way to refer to the control variable.
  control_variable = get_variable_storage(symbol, frame, hops)

  # Evaluate the initial and final bounds for the iteration.
  compiled_initial = exp_expression(
      ast.initial_value, symbols, frame, quoted_constants)
  compiled_final = exp_expression(
      ast.final_value, symbols, frame, quoted_constants)
  code = [t4050_compiled.Statement(code=compiled_initial.compute,
                                   stack_growth=compiled_initial.stack_growth),
          t4050_compiled.Statement(code=compiled_final.compute,
                                   stack_growth=compiled_final.stack_growth)]

  # If either bound leaves elements on the stack, we cannot GOTO anywhere
  # within the FOR loop body, since we won't have the chance to clean up the
  # stack before the jump.
  #
  # (A better compiler would be able to insert necessary cleanups in front of
  # the GOTO, but this is quite difficult as it depends on where the GOTO goes.
  # If it jumps in the FOR loop, there's nothing to clean up. If it jumps out
  # of the FOR loop, then we'll have to clean up one stack element. If it jumps
  # out of two nested FOR loops, then there are two stack elements to clean up.
  # But the analysis to count the correct amount of cleanup for each label is
  # complicated, so we don't implement it for now.
  def no_gotos_exits(ast: pascal_parser.AstNode, _):
    def error_text(forbidden: str):
      return (f'The 4050 BASIC target does not permit use of {forbidden} '
              'inside FOR loops whose initial or final control variable '
              'values are calculated by executing a function.')
    match ast:
      case pascal_parser.StatementGoto():
        raise RuntimeError(error_text('GOTO'))
      case pascal_parser.StatementProcedure():
        called = symbols[ast.call.binding]
        if isinstance(called, t4050_extensions.ExtensionExitSymbol):
          raise RuntimeError(error_text('Exit'))
  stack_growth = compiled_initial.stack_growth + compiled_final.stack_growth
  if stack_growth > 0: mupas_descent.depth_first(no_gotos_exits, ast.body, None)

  # If the control variable is a 4050 BASIC numeric variable, we can "optimise"
  # by using 4050 BASIC's native FOR loop instead of an IF and GO TO based
  # homebrew. But we must not allow the user to mutate the control variable,
  # because if they do, the program will crash.
  if isinstance(symbol.storage, t4050_resources.NumericVariable):
    # The way we accomplish this is cheeky: we use a new nested scope that
    # binds the symbol to a zero-argument extension function, making the
    # control variable readable but not writable.
    with symbols.nest('{FOR}') as symbols_for:

      # Implementation of the extension.
      def extension(
          args: list[t4050_compiled.Expression]
      ) -> t4050_compiled.Expression:
        if args: raise RuntimeError(f'{ast.control_variable} is a FOR loop '
                                    "control variable; you can't call it")
        assert isinstance(symbol, mupas_scopes.VariableSymbol)  # already known!
        return t4050_compiled.Expression(
            typeinfo=symbol.typeinfo,
            access=control_variable, is_unqualified_variable=True)

      # Binding the extension.
      symbols_for[ast.control_variable] = (
          t4050_extensions.ExtensionFunctionSymbol(extension=extension))

      # Now we can implement the FOR loop.
      step = f' STEP {ast.step}' if ast.step != 1 else ''
      code.append(t4050_compiled.Statement(code=[
          f'      FOR {control_variable}={compiled_initial.access} TO '
          f'{compiled_final.access}{step}']))
      code.append(sta_statement(
          ast.body, symbols_for, frame, quoted_constants, labels))
      code.append(
          t4050_compiled.Statement(code=[f'      NEXT {control_variable}']))

  # Regrettably (or maybe not!), we can't use the native FOR loop; let's roll
  # our own.
  else:
    if not isinstance(symbol.storage, t4050_resources.StackValue):
      raise _InternalError  # There shouldn't be any other type of storage!
    top_label = labels.generate('ForTop', symbols)
    end_label = labels.generate('ForEnd', symbols)
    # Top of loop. Skip to exit if control variable exits the final bound.
    compare = '>' if ast.step >= 0 else '<'
    code.append(t4050_compiled.Statement(code=[
        f'      {control_variable}={compiled_initial.access}',
        f'{top_label}:',
        f'      IF {control_variable}{compare}{compiled_final.access} THEN '
        f'|{end_label}|']))
    # Execute the loop body.
    code.append(sta_statement(
        ast.body, symbols, frame, quoted_constants, labels))
    # Bottom of loop: increment control variable and return to top of loop.
    code.append(t4050_compiled.Statement(code=[
        f'      {control_variable}={control_variable}{ast.step:+}',
        f'      GO TO |{top_label}|',
        f'{end_label}:']))

  # Clean any extra use of stack.
  frame.release_n(stack_growth)  # All works fine if growth is 0.
  delete_info = frame.delete_info(frame[-1])  # Frame entry doesn't matter.
  code.append(
      t4050_compiled.Statement(code=delete_info.for_n_places(stack_growth)))

  return t4050_compiled.chain_statements(code)


def sta_repeat(
    ast: pascal_parser.StatementRepeat,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for StatementRepeat AST nodes."""
  top_label = labels.generate('RepeatTop', symbols)
  end_label = labels.generate('RepeatEnd', symbols)
  statements = [t4050_compiled.Statement(code=[f'{top_label}:'])] + [
      sta_statement(s, symbols, frame, quoted_constants, labels)
      for s in ast.body]

  compiled_condition = exp_expression(
      ast.condition, symbols, frame, quoted_constants)
  t4050_types.check_assignment_or_parameter_compatibility(
      mupas_types.Boolean(), compiled_condition.typeinfo)

  # We may need to clean up the stack if the loop condition grows the stack.
  # Happily, "releases" and "deletions" are no-ops if we don't need to do that.
  delete_info = frame.delete_info(frame[-1])  # Frame entry doesn't matter.
  frame.release_n(compiled_condition.stack_growth)  # Bookkeeping cleanup.
  stack_pop_code = delete_info.for_n_places(compiled_condition.stack_growth)
  # At the bottom of the loop, decide whether to loop once more.
  statements.append(t4050_compiled.Statement(code=(
      compiled_condition.compute +
      [f'      IF {compiled_condition.access} THEN |{end_label}|'] +
      stack_pop_code +
      [f'      GO TO |{top_label}|',
       f'{end_label}:'] +
      stack_pop_code)))

  return t4050_compiled.chain_statements(statements)


def sta_while(
    ast: pascal_parser.StatementWhile,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for StatementWhile AST nodes."""
  # Start with labels and the condition
  top_label = labels.generate('WhileTop', symbols)
  end_label = labels.generate('WhileEnd', symbols)
  compiled_condition = exp_expression(
      ast.condition, symbols, frame, quoted_constants)
  t4050_types.check_assignment_or_parameter_compatibility(
      mupas_types.Boolean(), compiled_condition.typeinfo)

  # We may need to clean up the stack if the loop condition grows the stack.
  # Happily, "releases" and "deletions" are no-ops if we don't need to do that.
  delete_info = frame.delete_info(frame[-1])  # Frame entry doesn't matter.
  frame.release_n(compiled_condition.stack_growth)  # Bookkeeping cleanup.
  stack_pop_code = delete_info.for_n_places(compiled_condition.stack_growth)

  return t4050_compiled.chain_statements([
      # At the top of the loop: decide whether to loop at all.
      t4050_compiled.Statement(code=(
          [f'{top_label}:'] +
          compiled_condition.compute +
          [f'      IF NOT {compiled_condition.access} THEN |{end_label}|'] +
          stack_pop_code)),
      # Here's the loop body itself.
      sta_statement(ast.body, symbols, frame, quoted_constants, labels),
      # At the bottom of the loop: the jump back to top, plus cleanup.
      t4050_compiled.Statement(code=(
          [f'      GO TO |{top_label}|',
           f'{end_label}:'] +
          stack_pop_code))])


def sta_label_and_statement(
    ast: pascal_parser.LabelAndStatement,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    labels: Labels,
) -> t4050_compiled.Statement:
  """Generate: for LabelAndStatement AST nodes."""
  statements: list[t4050_compiled.Statement] = []

  if ast.label is not None:
    try:
      labels[ast.label].defined = True
    except KeyError: raise RuntimeError(
      f'Attempt to define label {ast.label}, which was not declared')
    label = labels.generate('Label', symbols, ast.label)
    statements.append(t4050_compiled.Statement(code=[f'{label}:']))

  if ast.statement is not None: statements.append(
      sta_statement(ast.statement, symbols, frame, quoted_constants, labels))

  return t4050_compiled.chain_statements(statements)


###################
### EXPRESSIONS ###
###################


def exp_expression(
    ast: pascal_parser.Expression,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  """Generate: for Expression AST nodes.

  Ordinarily the AST-node-to-code-fragment functions in this module aren't
  extensively commented, but as this one has an argument structure that's
  (nearly) replicated by most of the code generating functions for expressions,
  it seems useful to document those arguments here.

  Args:
    ast: Abstract syntax tree for a muPas statement.
    symbols: Symbol table for the statement's scope.
    frame: A muPas abstracted stack frame for the statement's scope.
    quoted_constants: Quoted constants for the program.

  Returns:
    A compiled code fragment for the expression.
  """
  match ast:
    case pascal_parser.ExpressionLeaf():
      return exp_expression_leaf(ast, symbols, frame, quoted_constants)
    case pascal_parser.ExpressionUnary():
      return exp_expression_unary(ast, symbols, frame, quoted_constants)
    case pascal_parser.ExpressionBinary():
      return exp_expression_binary(ast, symbols, frame, quoted_constants)
    case _:
      raise _UnexpectedParseTreeNode


def exp_expression_leaf(
    ast: pascal_parser.ExpressionLeaf,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  """Generate: for ExpressionLeaf AST nodes."""
  match ast.value:
    case pascal_parser.UnsignedInteger(number=number):
      # Unsigned integer literals: 1234
      return t4050_compiled.Expression(
          typeinfo=mupas_types.Integer(), access=str(number), is_literal=True)
    case pascal_parser.UnsignedReal(number=number):
      # Unsigned real number literals: 1.2e34
      return t4050_compiled.Expression(
          typeinfo=mupas_types.Real(), access=str(number), is_literal=True)
    case pascal_parser.QuotedConstant(index=index):
      # 'String literals'.
      value = quoted_constants[index]
      literal = '"' + value.replace('"', '""') + '"'  # 4050 BASIC escaping.
      return t4050_compiled.Expression(
          typeinfo=mupas_types.String(length=len(value)),
          access=literal, is_literal=True)
    case pascal_parser.BindingReferenceOrCall():
      # Variables, constants, function calls.
      return exp_binding_reference_or_call(
          ast.value, symbols, frame, quoted_constants)
    case _:
      raise _UnexpectedParseTreeNode


def exp_binding_reference_or_call(
    ast: pascal_parser.BindingReferenceOrCall,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  """Generate: for BindingReferenceOrCall in expression contexts."""
  symbol = symbols[ast.binding]
  match symbol:
    case mupas_scopes.VariableSymbol():
      # Variables: scalars, arrays, and array elements.
      return exp_variable_access(symbol, ast, symbols, frame, quoted_constants)
    case (mupas_scopes.ConstantIntegerSymbol(typeinfo=typeinfo, value=value) |
          mupas_scopes.ConstantRealSymbol(typeinfo=typeinfo, value=value)):
      # Numeric constants.
      return t4050_compiled.Expression(
          typeinfo=symbol.typeinfo, access=str(value),
          is_literal=True)  # Because `access` is a BASIC literal.
    case mupas_scopes.ConstantStringSymbol(typeinfo=typeinfo, storage=storage):
      # String constants.
      # TODO: Implement subscripting of string constants.
      assert isinstance(storage, t4050_resources.StringVariable)
      variable = storage.variable_name
      return t4050_compiled.Expression(
          typeinfo=symbol.typeinfo, access=variable,
          can_be_lhs=False, is_unqualified_variable=True,
          is_literal=False)  # Because `access` isn't a BASIC literal.
    case mupas_scopes.SubroutineSymbol():
      # Calls to functions.
      return exp_function_call(
          ast.binding, symbol, ast, symbols, frame, quoted_constants)
    case t4050_extensions.ExtensionFunctionSymbol():
      # Function-like extensions.
      parameters = extract_call_parameters(ast.binding, ast)
      return invoke_extension_as_expression(
          symbol, parameters, symbols, frame, quoted_constants)
    case _:
      raise _InternalError  # There shouldn't be any other type of symbol.


def exp_variable_access(
    symbol: mupas_scopes.VariableSymbol,
    ast: pascal_parser.BindingReferenceOrCall,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  """Generate: access to a variable (or an element of an array variable)."""
  if ast.qualifiers_and_parameters:
    return exp_array_element_access(
        symbol, ast, symbols, frame, quoted_constants)
  else:
    hops = symbols.itemhops(ast.binding)
    access = get_variable_storage(symbol, frame, hops)
    return t4050_compiled.Expression(
        typeinfo=symbol.typeinfo, access=access,
        can_be_lhs=True,
        is_unqualified_variable=isinstance(
            symbol.storage, (t4050_resources.NumericVariable,
                             t4050_resources.StringVariable)))


def exp_array_element_access(
    symbol: mupas_scopes.VariableSymbol,
    ast: pascal_parser.BindingReferenceOrCall,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  """Generate: access to a variable (or an element of an array variable)."""
  hops = symbols.itemhops(ast.binding)
  array_name = get_variable_storage(symbol, frame, hops)
  array_type = symbol.typeinfo

  # First, some checks.
  if len(ast.qualifiers_and_parameters) != 1: raise RuntimeError(
      f'Too many qualifiers on variable {array_name} for the 4050 BASIC target')
  qualifier = ast.qualifiers_and_parameters[0]
  if not isinstance(qualifier, pascal_parser.QualifierIndex):
      raise RuntimeError('muPas only supports indexing qualifiers on variables '
                         f'for the 4050 BASIC target, not {type(qualifier)}')
  if not isinstance(array_type, (mupas_types.Array1d, mupas_types.Array2d)):
      raise RuntimeError('muPas array indexing only works with array '
                         f'variables, not {array_type} variables')
  indices = qualifier.indices
  indices_needed = 1 if isinstance(array_type, mupas_types.Array1d) else 2
  if len(indices) != indices_needed: raise RuntimeError(
      f'Elements in the array {array_name} need {indices_needed} indices, not '
      f'{len(indices)}.')

  # Now compile the array indexing at last.
  compiled_indices = [exp_expression(index, symbols, frame, quoted_constants)
                      for index in indices]
  first_element_indices = (  # For shifting array indices to BASIC's 1-based.
      [array_type.index_typeinfo.lower_bound]
      if isinstance(array_type, mupas_types.Array1d) else
      [array_type.row_index_typeinfo.lower_bound,
       array_type.col_index_typeinfo.lower_bound])
  basic_indices = []
  stack_growth = 0
  for index_0, compiled_index in zip(first_element_indices, compiled_indices):
    basic_indices.append(f'{compiled_index.access}' if index_0 == 1 else
                         f'({compiled_index.access}){1-index_0:+}')
    stack_growth += compiled_index.stack_growth
  # That's it: now we can compute the final compiled expression.
  return t4050_compiled.Expression(
    typeinfo=array_type.value_typeinfo,
    access=f'{array_name}[{",".join(basic_indices)}]',
    compute=list(itertools.chain(*list(i.compute for i in compiled_indices))),
    stack_growth=stack_growth, can_be_lhs=True, is_unqualified_variable=False)


def exp_function_call(
    name: str,
    symbol: mupas_scopes.SubroutineSymbol,
    ast: pascal_parser.BindingReferenceOrCall,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  # First, verify that this is a function and collect its return type.
  if not isinstance(symbol.typeinfo, mupas_types.Function): raise RuntimeError(
      f'Attempt to use a {type(symbol.typeinfo)} in an expression: functions '
      'are the only subroutines that can be used in expressions')
  typeinfo = symbol.typeinfo.return_typeinfo

  # Extract parameters from the AST and do some checks.
  parameters = extract_call_parameters(name, ast)
  check_call_parameters_length(ast.binding, symbol, parameters)

  # Set up the stack for the function call, call the function, then do cleanup.
  compute, stack_value = setup_stack_for_subroutine_and_call(
      name, symbol, parameters, symbols, frame, quoted_constants,
      keep_return_value=True)

  # Calculate how to access the return value of this function call.
  stack = frame.allocator.stack_resource.variable_name
  fp = frame.allocator.frame_pointer_resource.variable_name
  maybe_offset = lambda x: f'{x:+}' if x else ''
  access = f'{stack}[{fp}{maybe_offset(stack_value.fp_offset)}]'

  # All done! Note that the stack has grown by 1, accommodating the return
  # value of the function on the stack.
  return t4050_compiled.Expression(
      typeinfo=typeinfo, access=access, compute=compute, stack_growth=1)


def exp_expression_unary(
    ast: pascal_parser.ExpressionUnary,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  """Generate: for ExpressionUnary AST nodes."""
  compiled_arg = exp_expression(
      ast.expression, symbols, frame, quoted_constants)
  # Any expression we return is no longer an unqualified variable or suitable
  # for use as an assignment left-hand side. But if it was a BASIC literal
  # before, it may still be a literal: see below.
  compiled_arg.can_be_lhs = False
  compiled_arg.is_unqualified_variable = False

  match ast.op:
    # We can't take addresses of anything in muPas for 4050 BASIC.
    case pascal_parser.UnaryOp.ADDRESS_OF:
      raise RuntimeError(
          'muPas quantities in 4050 BASIC are not pointer-addressable and have '
          'no address, so the @ operator cannot be used')

    # A logical not operation changes the data type to boolean.
    case pascal_parser.UnaryOp.LOGICAL_NOT:
      base_t = get_base_typeinfo(compiled_arg.typeinfo)
      if not isinstance(base_t, mupas_types.ScalarNumber): raise RuntimeError(
          f"A value of type {type(compiled_arg.typeinfo)} can't be logically "
          'inverted for the 4050 BASIC target with unary NOT')
      compiled_arg.access = f'(NOT {compiled_arg.access})'
      compiled_arg.typeinfo = update_base_typeinfo(
          enclosing_typeinfo=compiled_arg.typeinfo,
          new_base_typeinfo=mupas_types.Boolean())
      compiled_arg.is_literal = False  # No longer a BASIC literal.
      return compiled_arg

    # A negation operation converts booleans, chars, enumerations, and ranges
    # to plain integers. Other types remain the same.
    case pascal_parser.UnaryOp.NEGATE:
      base_t = get_base_typeinfo(compiled_arg.typeinfo)
      if not isinstance(base_t, mupas_types.ScalarNumber): raise RuntimeError(
          f"A value of type {type(compiled_arg.typeinfo)} can't be negated "
          'for the 4050 BASIC target with the unary - operator')

      if compiled_arg.is_literal:
        # For literal values, we can do the negation in Python, and the result
        # is still a BASIC literal. We try int first to avoid rounding errors.
        try:
          compiled_arg.access = str(-int(compiled_arg.access))
        except ValueError:
          compiled_arg.access = str(-float(compiled_arg.access))
      else:
        # For non-literal values, we're more conservative in our negation.
        compiled_arg.access = f'(-({compiled_arg.access}))'

      if not isinstance(base_t, (mupas_types.Real, mupas_types.Longint)):
        compiled_arg.typeinfo = update_base_typeinfo(
            enclosing_typeinfo=compiled_arg.typeinfo,
            new_base_typeinfo=mupas_types.Integer())
      return compiled_arg


def exp_expression_binary(
    ast: pascal_parser.ExpressionBinary,
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  """Generate: for ExpressionBinary AST nodes."""
  compiled_left = exp_expression(
      ast.expression_left, symbols, frame, quoted_constants)
  compiled_right = exp_expression(
      ast.expression_right, symbols, frame, quoted_constants)

  ### Determining the type of the result ###

  def dual_array_arguments_compatibility_check(
      typeinfo_left: mupas_types.Type, typeinfo_right: mupas_types.Type):
    """Raise an error if two array arguments to a binary op are incompatible."""
    # Abbreviation of an error message.
    def fail():
      raise RuntimeError(
          'Incompatible binary expression types for the 4050 BASIC target: '
          f'{typeinfo_left} and {typeinfo_right}')
    # Both arguments must be arrays.
    if not (isinstance(typeinfo_left, _ARRAY_TYPES) and
            isinstance(typeinfo_right, _ARRAY_TYPES)): return
    # Array dimensionality must match.
    if type(typeinfo_left) != type(typeinfo_right): fail()  # 1-D or 2-D?
    if isinstance(typeinfo_left, mupas_types.Array1d):  # If 1-D, check size.
      dim = lambda t: (
          t.index_typeinfo.upper_bound - t.index_typeinfo.lower_bound)
      if dim(typeinfo_left) != dim(typeinfo_right): fail()
    else:  # If 2-D, check height and width.
      rows = lambda t: (
          t.row_index_typeinfo.upper_bound - t.row_index_typeinfo.lower_bound)
      cols = lambda t: (
          t.col_index_typeinfo.upper_bound - t.col_index_typeinfo.lower_bound)
      if (rows(typeinfo_left) != rows(typeinfo_right) or
          cols(typeinfo_left) != cols(typeinfo_right)): fail()

  def result_typeinfo(
      typeinfo_left: mupas_types.Type, typeinfo_right: mupas_types.Type,
      op: pascal_parser.BinaryOp,
  ) -> mupas_types.Type:
    """Determine the result type of a binary operation."""
    # Split types into base types and an enclosing type. This makes the most
    # sense in the case of arrays: the base type is the element type, and the
    # enclosing type is the rank and dimension of the array. For scalars, only
    # the base type matters and the "enclosing type" is ultimately discarded.
    base_t_left = get_base_typeinfo(typeinfo_left)
    base_t_right = get_base_typeinfo(typeinfo_right)
    base_ts = (base_t_left, base_t_right)
    enclosing_t = (typeinfo_left if isinstance(typeinfo_left, _ARRAY_TYPES) else
                   typeinfo_right)
    # Now do some checks.
    dual_array_arguments_compatibility_check(typeinfo_left, typeinfo_right)
    if isinstance(enclosing_t, _ARRAY_TYPES):  # Does op work on arrays?
      if op in (pascal_parser.BinaryOp.DIVIDE_TO_INT,
                pascal_parser.BinaryOp.MODULO): raise RuntimeError(
          f"Binary operation {op} isn't available for arrays for the 4050 "
          'BASIC target')  # ...because they are hybrid operations.
    # Finally, compute the type of the expression's result.
    base_t_result: mupas_types.ScalarNumber
    if op == pascal_parser.BinaryOp.DIVIDE_TO_REAL:
      base_t_result = mupas_types.Real()
    elif op == pascal_parser.BinaryOp.DIVIDE_TO_INT:
      base_t_result = mupas_types.Integer()
    elif op in (pascal_parser.BinaryOp.LOGICAL_AND,
                pascal_parser.BinaryOp.LOGICAL_OR,
                pascal_parser.BinaryOp.COMPARE_EQ,
                pascal_parser.BinaryOp.COMPARE_LT,
                pascal_parser.BinaryOp.COMPARE_GT,
                pascal_parser.BinaryOp.COMPARE_LE,
                pascal_parser.BinaryOp.COMPARE_GE,
                pascal_parser.BinaryOp.COMPARE_NE):
      base_t_result = mupas_types.Boolean()
    else:
      # From this point down, we assume that we're doing an arithmetic op.
      # Note the precedence order here for implicit type conversion. Note also
      # how specialised integer types (enumerations, subranges) just have their
      # types relaxed to integers.
      for st in (mupas_types.Real, mupas_types.Longint, mupas_types.Integer,
                 mupas_types.Char, mupas_types.Boolean):
        if isinstance(base_t_left, st) or isinstance(base_t_right, st):
          base_t_result = st()
          break
      else:
        raise _InternalError  # Found no type!
    # The type of the result at last.
    return update_base_typeinfo(enclosing_t, base_t_result)

  typeinfo = result_typeinfo(
      compiled_left.typeinfo, compiled_left.typeinfo, ast.op)

  ### Calculation ###

  compute = compiled_left.compute + compiled_right.compute
  stack_growth = compiled_left.stack_growth + compiled_right.stack_growth

  ### "Access" --- where substantial calculation actually occurs.

  right, left = compiled_right.access, compiled_left.access  # Abbreviation
  if ast.op in _EASY_BINARY_OPS:
    access = f'({left}{_EASY_BINARY_OPS[ast.op]}{right})'
  elif ast.op == pascal_parser.BinaryOp.DIVIDE_TO_INT:
    access = f'(INT({left}/{right}))'
  elif ast.op == pascal_parser.BinaryOp.MODULO:
    access = f'({left}-(INT({left}/{right})))'
  else:
    raise _InternalError

  # That's all, folks! At last.
  return t4050_compiled.Expression(typeinfo=typeinfo, access=access,
                                   compute=compute, stack_growth=stack_growth)


def get_base_typeinfo(typeinfo: mupas_types.Type) -> mupas_types.Type:
  """If an array, the type of its elements, otherwise a pass-through."""
  if isinstance(typeinfo, _ARRAY_TYPES):
    return typeinfo.value_typeinfo
  else:
    return typeinfo


def update_base_typeinfo(
    enclosing_typeinfo: mupas_types.Type, new_base_typeinfo: mupas_types.Type,
) -> mupas_types.Type:
  """Update the element type if an array, otherwise pass new_base_typeinfo."""
  if isinstance(enclosing_typeinfo, _ARRAY_TYPES):
    return dataclasses.replace(enclosing_typeinfo,
                               value_typeinfo=new_base_typeinfo)
  else:
    return new_base_typeinfo


##########################
### USEFUL SHARED BITS ###
##########################


def get_integer_constant(
    ast: pascal_parser.Constant,
    symbols: mupas_scopes.SymbolScopeProtocol,
) -> int:
  """Retrieve the integer value of an integer constant.

  Args:
    ast: Constant AST node.
    symbols: Bindings for the current scope.

  Returns:
    The integer value of the Constant node.
  """
  # This function is an unmodified copy of an original in mupas_analyses.py,
  # unfortunately. As easy as it was to copy it here, let's leave it as a
  # TODO to do something more elegant soon.
  if isinstance(ast, pascal_parser.ConstantSignedInteger):
    return ast.number
  elif isinstance(ast, pascal_parser.ConstantIdentifier):
    bound_item = symbols[ast.text]
    if isinstance(bound_item, mupas_scopes.ConstantIntegerSymbol):
      return -bound_item.value if ast.negated else bound_item.value
    else: raise RuntimeError(
      f'The non-integer constant {ast.text} was referenced in a place where '
      'an integer constant was expected')
  else: raise RuntimeError(
    'A non-integer constant was found in a place where an integer constant '
    'was expected')


def get_variable_storage(
    symbol: mupas_scopes.VariableSymbol,
    frame: t4050_resources.Frame,
    hops: int,
) -> str:
  """Generate inline-able BASIC code that refers to a muPas stored value.

  Args:
    symbol: Symbol table binding for the variable.
    frame: Stack frame bookkeeping information for the context in which this
        variable is being accessed.
    hops: 0 if the variable is defined in the current scope, 1 if it's defined
        in the parent scope, 2 if defined in the grandparent scope, and so on.

  Returns:
    A string that can be used in 4050 BASIC code to access or change the
    variable's value.
  """
  match symbol.storage:
    case t4050_resources.StringVariable() | t4050_resources.NumericVariable():
      return symbol.storage.variable_name
    case t4050_resources.StackValue(fp_offset=fp_offset):
      stack = frame.allocator.stack_resource.variable_name
      fp = frame.allocator.frame_pointer_resource.variable_name
      for _ in range(hops): fp = f'{stack}[{fp}-1]'
      maybe_offset = lambda x: f'{x:+}' if x else ''
      return f'{stack}[{fp}{maybe_offset(fp_offset)}]'
    case _:
      raise _InternalError


def extract_call_parameters(
    name: str,
    ast: pascal_parser.BindingReferenceOrCall,
) -> tuple[pascal_parser.Expression]:
  """Extract subroutine or extension from the AST node

  Optionally performs some checking.

  Args:
    name: Name of the subroutine being invoked.
    symbol: Scope symbol referring to the subroutine or extension.
    ast: Parse tree node for the subroutine's invocation.
    check_length: If true, makes sure that the number of parameters provided
        in the code matches the number required by the subroutine or extension.

  Returns:
    The expressions that make up the subroutine's argument list.
  """
  if len(ast.qualifiers_and_parameters) > 1: raise RuntimeError(
      f'Too many qualifiers on the invocation of subroutine {name} for the '
      '4050 BASIC target')  # Remember, subroutines can't return arrays.
  qualifier = (  # Note special-case to handle no-argument subroutines.
      ast.qualifiers_and_parameters[0] if ast.qualifiers_and_parameters else
      pascal_parser.ParameterList(parameters=()))  # type: ignore  # mypy??
  if not isinstance(qualifier, pascal_parser.ParameterList): raise RuntimeError(
      'muPas only supports parameter list qualifiers on subroutines for the '
      f'4050 BASIC target, not {type(qualifier)}')
  return qualifier.parameters


def check_call_parameters_length(
    name: str,
    symbol: mupas_scopes.SubroutineSymbol,
    parameters: tuple[pascal_parser.Expression],
):
  if len(parameters) != len(symbol.typeinfo.parameters): raise RuntimeError(
      f'Subroutine {name} takes {len(symbol.typeinfo.parameters)} '
      f'parameters, not the {len(parameters)} provided.')


def setup_stack_for_subroutine_and_call(
    name: str,
    symbol: mupas_scopes.SubroutineSymbol,
    parameters: tuple[pascal_parser.Expression],
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
    keep_return_value: bool,
) -> tuple[list[str], t4050_resources.StackValue]:
  """Compile a subroutine call.

  Contains (mostly) common code for making calls to procedures and functions,
  and, in the latter case, in statement or expression contexts (i.e. in
  settings where you respectively don't and do wish to retain the return
  value).

  Args:
    name: Name of the subroutine being called.
    symbol: Scope symbol referring to the subroutine.
    parameters: Parse tree nodes for subroutine parameter expressions --- see
        `extract_call_parameters`.
    symbols: Symbol table for the scope enclosing this subroutine call.
    frame: Stack frame for the context in which this subroutine call occurs.
    quoted_constants: List of string literals.
    keep_return_value: For functions: whether to leave the return value on the
        stack. (For procedures: does nothing.) If True, the stack will grow by
        one place.

  Returns:
    A 2-tuple with these contents:
    * [0] Lines of 4050 BASIC assembly that prepare the stack for the
       subroutine call, then call the subroutine, then perform cleanup.
    * [1] Stack bookkeeping entry for the element left on the stack, if the
       subroutine is a function and keep_return_value is True. If neither is
       the case, this value is not meaningful and can be ignored/discarded.
  """
  # Useful aliases and values for later.
  stack = frame.allocator.stack_resource.variable_name
  sp = frame.allocator.stack_pointer_resource.variable_name
  fp = frame.allocator.frame_pointer_resource.variable_name
  subroutine_is_a_function = isinstance(symbol.typeinfo, mupas_types.Function)
  subroutine_frame = symbol.frame
  if not isinstance(subroutine_frame, t4050_resources.Frame):
    raise _InternalError  # Does mupas_analyses.get_symbols() have a bug?

  # Allocate space in our own internal bookkeeping for the values we're
  # preparing for the called subroutine's stack frame. These calls only make
  # room for the return value (if we're calling a function) and the two stack
  # maintenance values associated with all subroutine calls. Parameters will
  # be pushed onto the stack in a little while.
  if subroutine_is_a_function:
    stack_value = frame[frame.allocate_for_function_call()]
  else:
    stack_value = frame[frame.allocate_for_procedure_call()]

  # It's convenient right now to collect these objects that emit code for stack
  # allocation and deallocation.
  value_init_info = frame.init_info(stack_value)
  value_delete_info = frame.delete_info(stack_value)
  frame_init_info = subroutine_frame.allocator.init_info(subroutine_frame)
  frame_delete_info = subroutine_frame.allocator.delete_info(subroutine_frame)

  # Kick off the code by advancing the stack pointer to just beyond the
  # return value and stack maintenance values for the stack frame now under
  # construction. This is the stack frame complement to the
  # `frame.allocate_for...` bookkeeping methods called above.
  code = frame_init_info.advance_stack_pointer_to_parameters()

  # The stack pointer and the internal bookkeeping for the calling context's
  # stack frame are now in agreement. We now tell the helper that's helping
  # build the new stack frame to take note of where the stack maintenance
  # values can be found. (The helper will generate code to supply these values.
  frame_init_info.mark_stack_maintenance_offsets(frame)

  # We're now ready to start assembling subroutine parameters. Evaluate each of
  # the parameters and push them onto the stack.
  for param_ast, param_type in zip(parameters, symbol.typeinfo.parameters):
    # As an optimisation, we determine whether an argument is a bare function
    # call. If it is, we have less stack manipulation to do, since we can
    # arrange for the function call result to be deposited exactly where we
    # want it. Apologies for the messiness: it's all for mypy.
    param_is_function_call = False
    if isinstance(param_ast, pascal_parser.ExpressionLeaf):
      if isinstance(param_ast.value, pascal_parser.BindingReferenceOrCall):
        param_symbol = symbols[param_ast.value.binding]
        if isinstance(param_symbol, mupas_scopes.SubroutineSymbol):
          param_is_function_call = isinstance(
              param_symbol.typeinfo, mupas_types.Function)

    # If this is a function call, we can compile the parameter calculation
    # straightaway. There's no need to access anything since the result has
    # already been placed where we want on the stack.
    #
    # There's no need to update internal frame bookkeeping after the function
    # call, since there will be one extra leftover entry for the value that
    # *was* the called function's return value and is *now* the parameter.
    #
    # Similarly, there's no need to move the stack pointer, since after the
    # function call, it will have been rewound to one step *past* where it
    # was, leaving the return value on the stack.
    if param_is_function_call:
      param_compiled = exp_expression(
          param_ast, symbols, frame, quoted_constants)
      t4050_types.check_assignment_or_parameter_compatibility(  # Type checking.
          param_type.typeinfo, param_compiled.typeinfo)
      code.extend(param_compiled.compute)

    # If this isn't a function call, we first have to allocate the stack space
    # for the calculated parameter before we compile; then we can compile; then
    # we copy the result of the calculation to the place we just allocated.
    else:
      # Stack space allocation.
      param_value = frame[frame.allocate_for_subroutine_parameter()]
      code.extend(value_init_info.for_subroutine_parameter())
      # Expression calculation.
      param_compiled = exp_expression(
          param_ast, symbols, frame, quoted_constants)
      t4050_types.check_assignment_or_parameter_compatibility(  # Type checking.
          param_type.typeinfo, param_compiled.typeinfo)
      code.extend(param_compiled.compute)
      # Copying the result.
      code.append(  # Remember that .access may do additional calculations.
          f'      {stack}[{fp}+{param_value.fp_offset}]='
          f'{param_compiled.access}')
      # If the computation grew the stack, time to shrink it back down.
      code.extend(value_delete_info.for_n_places(param_compiled.stack_growth))

  # Now actually update the frame pointer to where it needs to be for the new
  # stack frame.
  code.extend(frame_init_info.setup_frame_pointer(
      common_scope_hops=symbols.itemhops(name)))  # TODO!!

  # Finally, advance the stack pointer to allocate stack space for any
  # stack-using local variables in the called subroutine. This completes the
  # preparation of the stack frame.
  num_variables_on_stack = (subroutine_frame.num_elements -
                            subroutine_frame.locals_position - len(parameters))
  assert num_variables_on_stack >= 0
  if num_variables_on_stack > 0:
    code.append(f'      {sp}={sp}+{num_variables_on_stack}')

  # Call the subroutine!
  subroutine_path = symbols.itempath(name).replace('/', '_')
  code.append(f'      GOS |_SubEnter{subroutine_path}|')

  # Cleanup. Get rid of stack frame bookkeeping we made for this call, except
  # for an entry for the function's return value (if the caller wants it). Then
  # retract the stack pointer back to just after that return value (if the
  # caller wants it --- otherwise go further). Since the return value has
  # already been placed where it needs to go, we can undertake the stack
  # pointer change without delay.
  num_parameters = len(parameters)
  if subroutine_is_a_function:
    if keep_return_value:
      frame.release_for_function_call_but_keep_result(num_parameters)
      code.extend(frame_delete_info.code(keep_result=True))
    else:
      frame.release_for_function_call(num_parameters)
      code.extend(frame_delete_info.code(keep_result=False))
  else:
    frame.release_for_procedure_call(num_parameters)
    code.extend(frame_delete_info.code(keep_result=False))

  return code, stack_value


def invoke_extension_as_statement(
    symbol: (t4050_extensions.ExtensionFunctionSymbol |
             t4050_extensions.ExtensionProcedureSymbol),
    parameters: tuple[pascal_parser.Expression],
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> list[str]:
  """Invokes an extension in a statement context.

  If the extension reports that values have been added to the stack, this
  routine adds code to remove those values.

  Args:
    symbol: Scope symbol referring to the extension.
    parameters: Parse tree nodes for extension parameter expressions --- see
        `extract_call_parameters`.
    symbols: Symbol table for the scope enclosing this extension invocation.
    frame: Stack frame for the context in which this invocation occurs.
    quoted_constants: List of string literals.

  Returns:
    Lines of code generated by a extension.
  """
  # Compile extension parameters.
  compiled_parameters = [
      exp_expression(param_ast, symbols, frame, quoted_constants)
      for param_ast in parameters]

  # Invoke the extension.
  code: list[str]
  stack_growth: int
  match symbol:
    case t4050_extensions.ExtensionProcedureSymbol(extension=extension):
      compiled_statement = extension(compiled_parameters)
      code = compiled_statement.code
      stack_growth = compiled_statement.stack_growth
    case t4050_extensions.ExtensionFunctionSymbol(extension=extension):
      compiled_expression = extension(compiled_parameters)
      code = compiled_expression.compute
      stack_growth = compiled_expression.stack_growth
    case _:
      raise _InternalError

  # Because this is a statement context, clean up any stack growth.
  delete_info = frame.delete_info(frame[-1])  # Frame entry doesn't matter.
  frame.release_n(stack_growth)  # Bookkeeping cleanup.
  stack_pop_code = delete_info.for_n_places(stack_growth)

  return code


def invoke_extension_as_expression(
    symbol: t4050_extensions.ExtensionFunctionSymbol,
    parameters: tuple[pascal_parser.Expression],
    symbols: mupas_scopes.SymbolScopeProtocol,
    frame: t4050_resources.Frame,
    quoted_constants: Sequence[str],
) -> t4050_compiled.Expression:
  """Invokes an extension in a statement context.

  Args:
    symbol: Scope symbol referring to the extension.
    parameters: Parse tree nodes for extension parameter expressions --- see
        `extract_call_parameters`.
    symbols: Symbol table for the scope enclosing this extension invocation.
    frame: Stack frame for the context in which this invocation occurs.
    quoted_constants: List of string literals.

  Returns:
    A compiled expression generated by the extension.
  """
  # Compile extension parameters.
  compiled_parameters = [
      exp_expression(param_ast, symbols, frame, quoted_constants)
      for param_ast in parameters]
  # Invoke the extension.
  return symbol.extension(compiled_parameters)


class _InternalError(Exception):
  """An exception type for any "this should never happen" situation."""


class _UnexpectedParseTreeNode(_InternalError):
  """Found a parse tree node we don't know how to handle."""
