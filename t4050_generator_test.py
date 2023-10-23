"""Tests for the t4050_generator module.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

Reflecting the fact that Tektronix 4050-series BASIC code generation was
written in a bit of a hurry, this test module is more than just unit tests
of code in `t4050_generator`. Some tests here are more like integration tests
for the entire compiler, or at least for 4050-series BASIC code generation in
particular.

This module is a rough record of the order in which certain code generation
functionality has been implemented, more or less.
"""
# pylint: disable=invalid-name  # for 's' as a shorthand for the stack array.

import dataclasses
import re
import unittest

import mupas_analyses
import mupas_descent
import mupas_scopes
import mupas_types
import preprocessor
import pascal_parser
import t4050_generator
import t4050_resources
import t4050_stdlib_extensions

from typing import Collection, Mapping, Optional, Sequence


###############
### Helpers ###
###############


@dataclasses.dataclass
class AllocatorsEtc:
  """A container for resource allocators and other useful data.

  An AllocatorsEtc object may be passed to parse_and_allocate_symbols if the
  allocators and the reverse call graph are required by a test. If not, you can
  omit this argument. If this seems like "tacked-on" functionality, you're not
  wrong...
  """
  static: Optional[t4050_resources.StaticAllocator] = None
  stack: Optional[t4050_resources.StackAllocator] = None
  frame: Optional[t4050_resources.FrameAllocator] = None
  rev_call_graph: Optional[Mapping[str, Collection[str]]] = None


def parse_and_allocate_symbols(
    source_text: str, allocators_etc: Optional[AllocatorsEtc] = None,
) -> tuple[pascal_parser.AstNode, mupas_scopes.SymbolScope,
           t4050_resources.Frame, Sequence[str]]:
  """Pre code-generation compile steps for (a part/all) of the parse tree."""
  # Parse the program. We won't bother with checks.
  preprocessed, quoted_constants = preprocessor.preprocess(source_text)[0:2]
  ast = pascal_parser.parse(preprocessed)
  assert isinstance(ast, pascal_parser.Program)  # No units, only programs.

  # Build symbol table, call graph, and reverse transitive call graph.
  extensions = t4050_stdlib_extensions.extensions()
  symbols = mupas_analyses.get_symbols(ast, quoted_constants, extras=extensions)
  call_graph = mupas_analyses.get_call_graph(ast, symbols)
  rev_call_graph = mupas_analyses.get_reverse_transitive_call_graph(call_graph)

  # Set up allocators.
  static_allocator = t4050_resources.StaticAllocator()
  stack_allocator = t4050_resources.StackAllocator(100, static_allocator)
  frame_allocator = stack_allocator.allocate()
  if allocators_etc is not None:
    allocators_etc.static = static_allocator
    allocators_etc.stack = stack_allocator
    allocators_etc.frame = frame_allocator
    allocators_etc.rev_call_graph = rev_call_graph

  # Add static and stack resources for the program (in that order!).
  mupas_analyses.add_static_resources(symbols, rev_call_graph, static_allocator)
  mupas_analyses.add_stack_resources(symbols, frame_allocator)

  # Create a fictional enclosing stack frame that mimics what you'd have for a
  # zero-parameter procedure with no local variables. For this, we need to
  # describe a muPas abstract stack frame for the frame allocator to allocate.
  frame_typeinfos = [mupas_types.CodePointer(),  # Return address.
                     mupas_types.DataPointer(),  # Old frame pointer.
                     mupas_types.DataPointer()]  # Enclosing scope pointer.
  frame = frame_allocator.allocate(frame_typeinfos)

  return ast, symbols, frame, quoted_constants


def get_node_by_type(ast: pascal_parser.AstNode, nodetype: type):
  """Retrieve an arbitrary note in ast that is an instance of nodetype."""
  def callback(ast: pascal_parser.AstNode, _):
    if isinstance(ast, nodetype):
      raise mupas_descent.Stop(ast)
  return mupas_descent.depth_first(callback, ast, None)


class LabelScanner:
  """Provide a way to recover automatically-generated labels for testing.

  Compiled output will use labels for program flow and control. These labels
  will be generated automatically, which makes it hard to predict compiler
  output in unit tests. This class can scan compiled output for label
  definitions so that those labels can be used when generating expected output.
  """
  def __init__(self, code: list[str]):
    """Initialise a LabelScanner with code that will be scanned for labels."""
    label_regex = re.compile(r'[A-Z_a-z]\w*:')
    self.labels = [c[:-1] for c in code if label_regex.fullmatch(c)]

  def nth_match(self, regex: str, label_number: int):
    """Return the n'th label (n > 0) matching regex or raise a ValueError."""
    for label in self.labels:
      if re.match(regex, label):
        if (label_number := label_number - 1) <= 0:
          return label
    else: raise ValueError(f'Failed to find a label matching {regex}')


#############
### Tests ###
#############


class T4050GeneratorTest(unittest.TestCase):
  """Test harness for testing the mupas_analyses module.

  Reflecting bad practice, this class basically became an integration test for
  the entire t4050 backend. I'd say this is T*D* to fix but then I'd get the
  linter on my case :-P
  """
  # pylint: disable=too-many-public-methods

  def test_unsigned_integer_in_expression(self):
    """The first and most basic test: can we compile an unsigned integer?"""
    source = """\
        PROGRAM AssignAnInt;
        VAR
          bar: Integer;
        BEGIN
          bar := 123;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Integer)
    self.assertEqual(comp.access, '123')
    self.assertFalse(comp.compute)

  def test_unsigned_real_in_expression(self):
    """Can we compile an unsigned real number?"""
    source = """\
        PROGRAM AssignAReal;
        VAR
          bar: Real;
        BEGIN
          bar := 3.141;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Real)
    self.assertEqual(comp.access, '3.141')
    self.assertFalse(comp.compute)

  def test_string_in_expression(self):
    """Can we compile a string literal?"""
    source = """\
        PROGRAM AssignAString;
        VAR
          bar: String[10];
        BEGIN
          bar := 'Say "Hi"!';
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.String)
    self.assertEqual(comp.access, '"Say ""Hi""!"')
    self.assertFalse(comp.compute)

  def test_unqualified_variable_reference_in_expression(self):
    """Can we compile a variable reference inside an expression?"""
    source = """\
        PROGRAM AssignAnIntVar;
        VAR
          foo, bar: Real;
        BEGIN
          bar := foo;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Real)
    self.assertIsInstance(  # Top-level scalars go in BASIC variables.
        symbols['foo'].storage, t4050_resources.NumericVariable)
    self.assertEqual(comp.access, symbols['foo'].storage.variable_name)
    self.assertFalse(comp.compute)

  def test_array1d_element_reference_in_expression(self):
    """Can we compile a 1-D array element access inside an expression?"""
    source = """\
        PROGRAM AssignAnArrayElement;
        VAR
          foo: ARRAY[2..5] OF Real;
          bar: Real;
        BEGIN
          bar := foo[3];
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Real)
    variable_name = symbols["foo"].storage.variable_name
    self.assertEqual(comp.access, f'{variable_name}[(3)-1]')
    self.assertFalse(comp.compute)

  def test_array2d_element_reference_in_expression(self):
    """Can we compile a 2-D array element access inside an expression?"""
    source = """\
        PROGRAM AssignAnArrayElement;
        VAR
          foo: ARRAY[2..5,1..9] OF Boolean;
          bar: Boolean;
        BEGIN
          bar := foo[3, 4];
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Boolean)
    variable_name = symbols["foo"].storage.variable_name
    self.assertEqual(comp.access, f'{variable_name}[(3)-1,4]')
    self.assertFalse(comp.compute)

  def test_constant_real_reference_in_expression(self):
    """Can we compile a constant numeric reference inside an expression?"""
    source = """\
        PROGRAM AssignAConstReal;
        CONST
          foo = -123.45;
        VAR
          bar: Real;
        BEGIN
          bar := foo;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Real)
    self.assertEqual(comp.access, '-123.45')
    self.assertFalse(comp.compute)

  def test_constant_string_reference_in_expression(self):
    """Can we compile a constant string reference inside an expression?"""
    source = """\
        PROGRAM AssignAConstReal;
        CONST
          foo = 'Tacos';
        VAR
          bar: String[15];
        BEGIN
          bar := foo;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.String)
    # Note that string constants go into string variables.
    self.assertEqual(comp.access, symbols['foo'].storage.variable_name)
    self.assertFalse(comp.compute)

  def test_unary_negation_expression(self):
    """Can we compile expressions that use a unary negation?"""
    source = """\
        PROGRAM AssignAnInt;
        VAR
          bar: Integer;
        BEGIN
          bar := -123;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    unary = get_node_by_type(ast, pascal_parser.ExpressionUnary)
    comp = t4050_generator.exp_expression(
        unary, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Integer)
    self.assertEqual(comp.access, '-123')
    self.assertFalse(comp.compute)

  def test_unary_logical_inversion_expression(self):
    """Can we compile expressions that use a unary logical inversion?"""
    source = """\
        PROGRAM AssignABool;
        VAR
          bar: Boolean;
        BEGIN
          bar := not 123;  { Implicit type conversion }
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    unary = get_node_by_type(ast, pascal_parser.ExpressionUnary)
    comp = t4050_generator.exp_expression(
        unary, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Boolean)
    self.assertEqual(comp.access, '(NOT 123)')
    self.assertFalse(comp.compute)

  def test_basic_binary_arithmetic_expression(self):
    """Can we compile expressions that make use of binary arithmetic?"""
    source = """\
        PROGRAM AssignYetAnotherInteger;
        VAR
          bar: Integer;
        BEGIN
          bar := 4 * (15 - 24);
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    binary = get_node_by_type(ast, pascal_parser.ExpressionBinary)
    comp = t4050_generator.exp_expression(
        binary, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Integer)
    self.assertEqual(comp.access, '(4*(15-24))')
    self.assertFalse(comp.compute)

  def test_basic_function_call_in_expression(self):
    """Can we compile expressions that call a function?"""
    source = """\
        PROGRAM AssignAFunctionResult;
        VAR
          bar: Integer;
        FUNCTION Foo(a: Integer; b: Real): Integer; BEGIN END;  { No body! }
        { Main program }
        BEGIN
          bar := Foo(5, 1.5);
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Integer)

    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    # Function call result will be at fp+0, since our enclosing scope is like
    # a procedure with no parameters or locals.
    self.assertEqual(comp.access, f'{s}[{fp}]')
    # Now to compare generated code against a kind of hand-assembly.
    self.assertEqual(  # Don't worry, we'll optimise some of this silly stuff...
        [c.strip() for c in comp.compute],
        [f'{sp}={sp}+3',       # Advance stack pointer to parameters.
         f'{sp}={sp}+1',       # Allocate space for the first parameter.
         f'{s}[{fp}+3]=5',     # Push 5 as the first parameter.
         f'{sp}={sp}+1',       # Allocate stack space for the next parameter.
         f'{s}[{fp}+4]=1.5',   # Push 1.5 as the second parameter.
         f'{s}[{fp}+1]={fp}',  # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',  # Enclosing scope context for call.
         f'{fp}={fp}+3',       # Advance frame pointer for call.
         'GOS |_SubEnter_AssignAFunctionResult_Foo|',  # Jump to subroutine.
         f'{fp}={s}[{fp}-2]',  # Restore old frame pointer after call.
         f'{sp}={sp}-4'])      # Pop all but the function return value...
    self.assertEqual(comp.stack_growth, 1)  # which remains on the stack.

  def test_complicated_function_call_in_expression(self):
    """Can we compile expressions with complicated function calls inside?"""
    source = """\
        PROGRAM AssignAFunctionResult;
        VAR
          bar: Integer;
        FUNCTION Foo(a: Integer; b: Real): Integer; BEGIN END;  { No body! }
        FUNCTION Gamma(x: Integer): Real; BEGIN END;
        FUNCTION Five: Integer; BEGIN END;
        { Main program }
        BEGIN
          bar := Foo(Five, 1.5+Gamma(2));
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    leaf = get_node_by_type(ast, pascal_parser.ExpressionLeaf)
    comp = t4050_generator.exp_expression(
        leaf, symbols, frame, quoted_constants)
    self.assertIsInstance(comp.typeinfo, mupas_types.Integer)

    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    # Function call result will be at fp+0, since our enclosing scope is like
    # a procedure with no parameters or locals.
    self.assertEqual(comp.access, f'{s}[{fp}]')
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(  # Don't worry, we'll optimise some of this silly stuff...
        [c.strip() for c in comp.compute],
        [f'{sp}={sp}+3',    # Advance stack pointer to (Foo's) parameters.

         f'{sp}={sp}+3',    # Advance stack pointer to (Five's) parameters.
         f'{s}[{fp}+4]={fp}',  # Save old frame pointer for (Five's) call.
         f'{s}[{fp}+5]={fp}',  # Enclosing scope context for (Five's) call.
         f'{fp}={fp}+6',    # Advance frame pointer for (Five's) call.
         'GOS |_SubEnter_AssignAFunctionResult_Five|',  # Call Five
         f'{fp}={s}[{fp}-2]',  # Restore old frame pointer after (Five's) call.
         f'{sp}={sp}-2',    # Leave Five's result on the stack.
         f'{sp}={sp}+1',    # Advance SP to allocate space for parameter 2.

         f'{sp}={sp}+3',    # Advance stack pointer to (Gamma's) parameters.
         f'{sp}={sp}+1',    # Advance SP past the parameter we'er about to add.
         f'{s}[{fp}+8]=2',  # Supply the parameter to Gamma.
         f'{s}[{fp}+6]={fp}',  # Save old frame pointer for (Gamma's) call.
         f'{s}[{fp}+7]={fp}',  # Enclosing scope context for (Gamma's) call.
         f'{fp}={fp}+8',    # Advance frame pointer for (Gamma's) call.
         'GOS |_SubEnter_AssignAFunctionResult_Gamma|',  # Call Gamma.
         f'{fp}={s}[{fp}-2]',  # Restore old frame pointer after (Gamma's) call.
         f'{sp}={sp}-3',    # Leave Gamma's result on the stack.

         f'{s}[{fp}+4]=(1.5+{s}[{fp}+5])',  # Compute param 2 based on result.
         f'{sp}={sp}-1',    # Pop the Gamma's result, too.
         f'{s}[{fp}+1]={fp}',  # Save old frame pointer for (Foo's) call.
         f'{s}[{fp}+2]={fp}',  # Enclosing scope context for (Foo's) call.
         f'{fp}={fp}+3',    # Advance frame pointer for (Foo's) call.
         'GOS |_SubEnter_AssignAFunctionResult_Foo|',  # Call Foo at last.
         f'{fp}={s}[{fp}-2]',  # Restore old frame pointer after (Foo's) call.
         f'{sp}={sp}-4'])   # Leave Foo's result on the stack.
    self.assertEqual(comp.stack_growth, 1)  # which remains on the stack.

  def test_simple_assignment_statement(self):
    """The first and most basic test: can we compile an integer assignment?"""
    source = """\
        PROGRAM AssignAnInt;
        VAR
          bar: Integer;
        BEGIN
          bar := 123;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    assignment = get_node_by_type(ast, pascal_parser.StatementAssignment)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      assignment, symbols, frame, quoted_constants, labels)
    variable_name = symbols['bar'].storage.variable_name
    self.assertEqual([c.strip() for c in comp.code], [f'{variable_name}=123'])

  def test_goto_statement(self):
    """Can we compile a GOTO statement? (Should we? is a different question.)"""
    source = """\
        PROGRAM InfiniteLoop;
        LABEL
          123, 456;
        BEGIN
          123: GOTO 123;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    goto = get_node_by_type(ast, pascal_parser.StatementGoto)
    labels = t4050_generator.Labels()
    labels[123] = t4050_generator.Label(
        labels.generate('Label', symbols, 123), defined=True)
    labels[456] = t4050_generator.Label(labels.generate('Label', symbols, 456))
    self.assertFalse(labels[123].used)
    comp = t4050_generator.sta_statement(
      goto, symbols, frame, quoted_constants, labels)
    self.assertEqual([c.strip() for c in comp.code],
                     [f'GO TO |{labels[123].name}|'])
    self.assertTrue(labels[123].used)

  def test_if_statement(self):
    """Can we compile IF statements?"""
    source = """\
        PROGRAM IfsALot;
        VAR
          bar: Integer;
        BEGIN
          IF 1 > 2 THEN bar := 123;
          IF 1 > 3 THEN bar := 123 ELSE bar := 456;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    compound = get_node_by_type(ast, pascal_parser.StatementCompound)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      compound, symbols, frame, quoted_constants, labels)
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(comp.code)
    label_endif_1 = label_scanner.nth_match('_EndIf_IfsALot', 1)
    label_then_1 = label_scanner.nth_match('_Then_IfsALot', 1)
    label_endif_2 = label_scanner.nth_match('_EndIf_IfsALot', 2)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'IF NOT (1>2) THE |{label_endif_1}|',  # Jump UNLESS condition holds.
         f'{variable_name}=123',                 # Consequent.
         f'{label_endif_1}:',                    # Target to jump to.
         f'IF (1>3) THE |{label_then_1}|',       # Jump IF condition holds.
         f'{variable_name}=456',                 # Alternative.
         f'GO TO |{label_endif_2}|',             # Go past consequent.
         f'{label_then_1}:',                     # Target to jump to.
         f'{variable_name}=123',                 # Consequent.
         f'{label_endif_2}:'])                   # End of IF statement.

  def test_statements_that_grow_the_stack(self):
    """Will we clean up after statements that grow the stack?"""
    source = """\
        PROGRAM AssignFunctionStuff;
        VAR
          bar: Integer;
        FUNCTION Five: Integer; BEGIN END;  { No body! }
        BEGIN
          IF Five THEN bar := Five + Five ELSE bar := 123;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    compound = get_node_by_type(ast, pascal_parser.StatementIf)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      compound, symbols, frame, quoted_constants, labels)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(comp.code)
    label_then_1 = label_scanner.nth_match('_Then_AssignFunction', 1)
    label_endif_1 = label_scanner.nth_match('_EndIf_AssignFunction', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'{sp}={sp}+3',          # Advance stack pointer to parameters.
         f'{s}[{fp}+1]={fp}',     # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',     # Enclosing scope context for call.
         f'{fp}={fp}+3',          # Advance frame pointer for call.
         'GOS |_SubEnter_AssignFunctionStuff_Five|',  # Call the function.
         f'{fp}={s}[{fp}-2]',     # Restore old frame pointer after call.
         f'{sp}={sp}-2',          # Leave the result on the stack.

         f'IF {s}[{fp}] THE |{label_then_1}|',  # Jump if condition.

         f'{sp}={sp}-1',          # Alternative. Pop condition off stack.
         f'{variable_name}=123',  # Alternative body.
         f'GO TO |{label_endif_1}|',   # Jump past consequent.

         f'{label_then_1}:',      # Beginning of consequent.
         f'{sp}={sp}-1',          # Pop condition result off the stack.

         f'{sp}={sp}+3',          # Advance stack pointer to parameters.
         f'{s}[{fp}+1]={fp}',     # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',     # Enclosing scope context for call.
         f'{fp}={fp}+3',          # Advance frame pointer for call.
         'GOS |_SubEnter_AssignFunctionStuff_Five|',  # Expression term call
         f'{fp}={s}[{fp}-2]',     # Restore old frame pointer after call.
         f'{sp}={sp}-2',          # Leave its result on the stack.

         f'{sp}={sp}+3',          # Advance stack pointer for call.
         f'{s}[{fp}+2]={fp}',     # Save old frame pointer for call.
         f'{s}[{fp}+3]={fp}',     # Enclosing scope context for call.
         f'{fp}={fp}+4',          # Advance frame pointer for call.
         'GOS |_SubEnter_AssignFunctionStuff_Five|',  # Expression term call
         f'{fp}={s}[{fp}-2]',     # Restore old frame pointer after call.
         f'{sp}={sp}-2',          # Leave its result on the stack.

         f'{variable_name}=({s}[{fp}]+{s}[{fp}+1])',  # Add expression terms.
         f'{sp}={sp}-2',          # Pop both results off the stack.

         f'{label_endif_1}:'])    # Jump target for the alternative.

  def test_repeat_statement(self):
    """Can we compile REPEAT statements?"""
    source = """\
        PROGRAM RepeatsALot;
        VAR
          bar: Integer;
        FUNCTION Five: Integer; BEGIN END;  { No body! }
        BEGIN
          REPEAT
            bar := 123;
            REPEAT
              bar := 456;
            UNTIL Five;
          UNTIL 1;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    repeat = get_node_by_type(ast, pascal_parser.StatementRepeat)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      repeat, symbols, frame, quoted_constants, labels)
    variable_name = symbols['bar'].storage.variable_name
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(comp.code)
    label_top_1 = label_scanner.nth_match('_RepeatTop_RepeatsALot', 1)
    label_end_1 = label_scanner.nth_match('_RepeatEnd_RepeatsALot', 1)
    label_top_2 = label_scanner.nth_match('_RepeatTop_RepeatsALot', 2)
    label_end_2 = label_scanner.nth_match('_RepeatEnd_RepeatsALot', 2)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'{label_top_1}:',             # Top of outer loop.
         f'{variable_name}=123',        # Outer loop body first statement.
         f'{label_top_2}:',             # Top of inner loop.
         f'{variable_name}=456',        # Inner loop body.
         f'{sp}={sp}+3',                # Advance stack pointer to parameters.
         f'{s}[{fp}+1]={fp}',           # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',           # Enclosing scope context for call.
         f'{fp}={fp}+3',                # Advance frame pointer for call.
         'GOS |_SubEnter_RepeatsALot_Five|',  # Call condition function.
         f'{fp}={s}[{fp}-2]',           # Restore old frame pointer after call.
         f'{sp}={sp}-2',                # Leave the result on the stack.
         f'IF {s}[{fp}] THE |{label_end_1}|',   # Leave if condition is true.
         f'{sp}={sp}-1',                # Else, pop condition result off stack.
         f'GO TO |{label_top_2}|',      # Go back to repeat inner loop.
         f'{label_end_1}:',             # Off-ramp for the inner loop.
         f'{sp}={sp}-1',                # Pop condition result off stack.
         f'IF 1 THE |{label_end_2}|',   # Leave outer loop if condition is true.
         f'GO TO |{label_top_1}|',      # Wasn't, so repeat outer loop.
         f'{label_end_2}:'])            # Repeat statement exit.

  def test_while_statement(self):
    """Can we compile WHILE statements?"""
    source = """\
        PROGRAM WhilesALot;
        VAR
          bar: Integer;
        FUNCTION Five: Integer; BEGIN END;  { No body! }
        BEGIN
          WHILE 1 DO BEGIN
            bar := 123;
            WHILE Five - 4 DO bar := 456;
          END;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    ast_while = get_node_by_type(ast, pascal_parser.StatementWhile)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      ast_while, symbols, frame, quoted_constants, labels)
    variable_name = symbols['bar'].storage.variable_name
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(comp.code)
    label_top_1 = label_scanner.nth_match('_WhileTop_WhilesALot', 1)
    label_end_1 = label_scanner.nth_match('_WhileEnd_WhilesALot', 1)
    label_top_2 = label_scanner.nth_match('_WhileTop_WhilesALot', 2)
    label_end_2 = label_scanner.nth_match('_WhileEnd_WhilesALot', 2)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'{label_top_1}:',         # Outer loop top.
         f'IF NOT 1 THE |{label_end_2}|',   # If condition not met, skip out.
         f'{variable_name}=123',    # Outer loop body first statement.
         f'{label_top_2}:',         # Inner loop top.
         f'{sp}={sp}+3',            # Advance stack pointer to parameters.
         f'{s}[{fp}+1]={fp}',       # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',       # Enclosing scope context for call.
         f'{fp}={fp}+3',            # Advance frame pointer for call.
         'GOS |_SubEnter_WhilesALot_Five|',  # Call condition function.
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-2',            # Leave the result on the stack.
         f'IF NOT ({s}[{fp}]-4) THE |{label_end_1}|',   # Exit inner if unmet.
         f'{sp}={sp}-1',            # Otherwise, pop result off the stack.
         f'{variable_name}=456',    # Inner loop body.
         f'GO TO |{label_top_2}|',  # Back to top of the inner loop.
         f'{label_end_1}:',         # Inner loop exit.
         f'{sp}={sp}-1',            # Pop condition result off the stack.
         f'GO TO |{label_top_1}|',  # Back to top of the outer loop.
         f'{label_end_2}:'])        # Outer loop exit.

  def test_case_statement(self):
    """Can we compile CASE statements?"""
    source = """\
        PROGRAM Casey;
        VAR
          bar: Integer;
        FUNCTION Five: Integer; BEGIN END;  { No body! }
        BEGIN
          CASE Five OF
            1,2,3,4:  bar := 123;
            5:        bar := 456;
            otherwise bar := 789;
          END;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    ast_case = get_node_by_type(ast, pascal_parser.StatementCase)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      ast_case, symbols, frame, quoted_constants, labels)
    variable_name = symbols['bar'].storage.variable_name
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(comp.code)
    label_case_1 = label_scanner.nth_match('_Case1_Casey', 1)
    label_case_2 = label_scanner.nth_match('_Case2_Casey', 1)
    label_end_1 = label_scanner.nth_match('_CaseEnd_Casey', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'{sp}={sp}+3',            # Advance stack pointer to parameters.
         f'{s}[{fp}+1]={fp}',       # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',       # Enclosing scope context for call.
         f'{fp}={fp}+3',            # Advance frame pointer for call.
         'GOS |_SubEnter_Casey_Five|',  # Call condition function.
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-2',            # Leave the result on the stack.

         f'IF {s}[{fp}]=1 OR {s}[{fp}]=2 OR {s}[{fp}]=3 OR {s}[{fp}]=4 '
         f'THE |{label_case_1}|',   # Where to jump for the first case.
         f'IF {s}[{fp}]=5 THE |{label_case_2}|',  # Where for the second case.

         f'{sp}={sp}-1',            # Pop condition result off the stack.
         f'{variable_name}=789',    # Otherwise case body.
         f'GO TO |{label_end_1}|',  # Jump beyond case statement.

         f'{label_case_1}:',        # Jump target for first case.
         f'{sp}={sp}-1',            # Pop condition result off the stack.
         f'{variable_name}=123',    # First case body.
         f'GO TO |{label_end_1}|',  # Jump beyond case statement.

         f'{label_case_2}:',        # Jump target for second case.
         f'{sp}={sp}-1',            # Pop condition result off the stack.
         f'{variable_name}=456',    # Second case body. Then fall through to...

         f'{label_end_1}:'])        # Case statement exit.

  def test_procedure_statement(self):
    """Can we compile statements that are subroutine calls?"""
    source = """\
        PROGRAM CallsALot;
        FUNCTION Five: Integer; BEGIN END;  { No body! }
        PROCEDURE Hi(a, b: Real); BEGIN END;  { Also no body! }
        BEGIN
          { REPEAT makes the calls easier to extract with get_node_by_type }
          REPEAT
            Five;
            Hi(1.1, 2.2);
          UNTIL 0;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    ast_repeat = get_node_by_type(ast, pascal_parser.StatementRepeat)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      ast_repeat, symbols, frame, quoted_constants, labels)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    # Collect labels.
    label_scanner = LabelScanner(comp.code)
    label_top_1 = label_scanner.nth_match('_RepeatTop_CallsALot', 1)
    label_end_1 = label_scanner.nth_match('_RepeatEnd_CallsALot', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'{label_top_1}:',         # Top of the loop.

         f'{sp}={sp}+3',            # Advance stack pointer to parameters.
         f'{s}[{fp}+1]={fp}',       # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',       # Enclosing scope context for call.
         f'{fp}={fp}+3',            # Advance frame pointer for call.
         'GOS |_SubEnter_CallsALot_Five|',  # Call function.
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-3',            # Pop everything off the stack.

         f'{sp}={sp}+2',            # Advance stack pointer to parameters
         f'{sp}={sp}+1',            # Allocate space for the first parameter.
         f'{s}[{fp}+2]=1.1',        # Push first parameter.
         f'{sp}={sp}+1',            # Allocate space for the first parameter.
         f'{s}[{fp}+3]=2.2',        # Push second parameter.
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={fp}',       # Enclosing scope context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         'GOS |_SubEnter_CallsALot_Hi|',  # Call procedure
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-4',            # Pop everything off the stack.

         f'IF 0 THE |{label_end_1}|',   # Leave loop if condition is true.
         f'GO TO |{label_top_1}|',  # Wasn't, so repeat loop.
         f'{label_end_1}:'])        # Repeat statement exit.

  def test_for_statement_that_uses_native_for(self):
    """Can we compile FOR statements that can use native 4054 BASIC FOR?"""
    source = """\
        PROGRAM For;
        VAR
          bar, i: Integer;
        BEGIN
          FOR i := 1 TO 10 DO bar := i;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    ast_for = get_node_by_type(ast, pascal_parser.StatementFor)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      ast_for, symbols, frame, quoted_constants, labels)
    # Collect storage information.
    bar_name = symbols['bar'].storage.variable_name
    i_name = symbols["i"].storage.variable_name
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'FOR {i_name}=1 TO 10',
         f'{bar_name}={i_name}',
         f'NEX {i_name}'])

  def test_for_statement_that_uses_native_for_backwards(self):
    """Can we compile FOR statements that using native FOR, again?"""
    source = """\
        PROGRAM Rof;
        VAR
          bar, i: Integer;
        BEGIN
          FOR i := 10 DOWNTO 1 DO bar := i;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    ast_for = get_node_by_type(ast, pascal_parser.StatementFor)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      ast_for, symbols, frame, quoted_constants, labels)
    # Collect storage information.
    bar_name = symbols['bar'].storage.variable_name
    i_name = symbols["i"].storage.variable_name
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'FOR {i_name}=10 TO 1 STE -1',
         f'{bar_name}={i_name}',
         f'NEX {i_name}'])

  def test_call_with_local_variable(self):
    """Can we compile calls to subroutines with local variables?"""
    source = """\
        PROGRAM CallsOne;
        FUNCTION Five: Integer; VAR x: Real; BEGIN END;  { No body! }
        BEGIN
          Five;
        END."""
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(source)
    ast_procedure = get_node_by_type(ast, pascal_parser.StatementProcedure)
    labels = t4050_generator.Labels()
    comp = t4050_generator.sta_statement(
      ast_procedure, symbols, frame, quoted_constants, labels)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in comp.code],
        [f'{sp}={sp}+3',          # Advance stack pointer to parameters.
         f'{s}[{fp}+1]={fp}',     # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',     # Enclosing scope context for call.
         f'{fp}={fp}+3',          # Advance frame pointer for call.
         f'{sp}={sp}+1',          # Grow stack for local variables.
         'GOS |_SubEnter_CallsOne_Five|',  # Call the function.
         f'{fp}={s}[{fp}-2]',     # Restore old frame pointer after call.
         f'{sp}={sp}-4'])         # Leave nothing on the stack.

  def test_program_00(self):
    """Can we compile an entire, very empty program?"""
    source = """\
        PROGRAM Empty;
        BEGIN
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_Empty|',  # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_Empty:',         # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.
         '_Exit_Empty:',            # Program exit label.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # That's all, folks!

  def test_program_01(self):
    """Can we compile an entire program of fairly slight complexity?"""
    source = """\
        PROGRAM Basic;
        VAR
          bar: Real;
        { This procedure: takes a param, accesses a value outside its scope. }
        PROCEDURE SetX(val: Real); BEGIN bar := val; END;
        BEGIN
          SetX(123.45);
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(code)
    label_end_1 = label_scanner.nth_match('_BlockEnd_Basic', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_Basic|',  # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_Basic:',         # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{sp}={sp}+2',            # Advance stack pointer to parameters.
         f'{sp}={sp}+1',            # Allocate space for the parameter.
         f'{s}[{fp}+2]=123.45',     # Push first parameter.
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         'GOS |_SubEnter_Basic_SetX|',  # Call procedure.
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-3',            # Pop everything off stack.

         '_Exit_Basic:',            # Program exit label.
         f'DEL {variable_name}',    # Cleanup local variable.
         f'GO TO |{label_end_1}|',  # Jump to end of program.

         'REM /Basic/SetX',
         '_SubEnter_Basic_SetX:',   # SetX subroutine implementation.
         f'{variable_name}={s}[{fp}]',  # Subroutine body (an assignment).
         '_Exit_Basic_SetX:',       # SetX subroutine exit label.
         'RET',                     # Return to caller.

         f'{label_end_1}:',         # End of program.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # End of program.

  def test_program_02(self):
    """Can we compile an entire program of marginal complexity?"""
    source = """\
        PROGRAM SoSo;
        VAR
          bar: Real;

        PROCEDURE A(val: Real);
          PROCEDURE B(val: Real); BEGIN bar := val; END;
        BEGIN
          B(val);
        END;

        BEGIN
          A(123.45);
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(code)
    label_end_1 = label_scanner.nth_match('_BlockEnd_SoSo_A', 1)
    label_end_2 = label_scanner.nth_match('_BlockEnd_SoSo_0', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_SoSo|',   # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_SoSo:',          # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{sp}={sp}+2',            # Advance stack pointer to "A" parameters.
         f'{sp}={sp}+1',            # Allocate space for the parameter.
         f'{s}[{fp}+2]=123.45',     # Push first parameter.
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         'GOS |_SubEnter_SoSo_A|',  # Call procedure "A".
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-3',            # Pop everything off stack.

         '_Exit_SoSo:',             # Program exit label.
         f'DEL {variable_name}',    # Cleanup local variable.
         f'GO TO |{label_end_2}|',  # Jump to end of program.

         'REM /SoSo/A',
         '_SubEnter_SoSo_A:',       # "A" subroutine implementation.
         f'{sp}={sp}+2',            # Advance stack pointer to "B" parameters.
         f'{sp}={sp}+1',            # Allocate space for the parameter.
         f'{s}[{fp}+3]={s}[{fp}]',  # Push first parameter.
         f'{s}[{fp}+1]={fp}',       # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+3',            # Advance frame pointer for call.
         'GOS |_SubEnter_SoSo_A_B|',  # Call procedure "B".
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-3',            # Pop everything off stack.
         '_Exit_SoSo_A:',           # "A" subroutine exit label.
         f'GO TO |{label_end_1}|',  # Jump to end of "A".

         'REM /SoSo/A/B',
         '_SubEnter_SoSo_A_B:',     # "B" subroutine implementation.
         f'{variable_name}={s}[{fp}]',  # Subroutine body (an assignment).
         '_Exit_SoSo_A_B:',         # "B" subroutine exit label..
         'RET',                     # Return to caller.

         f'{label_end_1}:',         # End of "A".
         'RET',                     # Return to caller.

         f'{label_end_2}:',         # Program exit point.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # End of program.

  def test_program_03(self):
    """Can we compile a program with string constants?"""
    source = """\
        PROGRAM WithStrings;
        CONST
          a = '"Hi there" ';
          b = 5;
          c = 'World''s';
        VAR
          bar: Real;
        BEGIN
          bar := b;
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    hithere_name = symbols['a'].storage.variable_name
    worlds_name = symbols['c'].storage.variable_name
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_WithStrings|',  # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_WithStrings:',   # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {hithere_name}(11)',   # Dimension string constant "a".
         f'DIM {worlds_name}(7)',     # Dimension string constant "b".
         f'{hithere_name}="""Hi there"" "',  # Define string constant "a".
         f'{worlds_name}="World\'s"',        # Define string constant "b".
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{variable_name}=5',      # Program body: do an assignment.

         '_Exit_WithStrings:',      # Program exit label.
         f'DEL {variable_name}',    # Cleanup local variable.
         f'DEL {hithere_name}',     # Delete string constant "a".
         f'DEL {worlds_name}',      # Delete string constant "b".
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # That's all, folks!

  def test_program_04(self):
    """Can we compile a program with a local variable on the stack?"""
    source = """\
        PROGRAM VarOnStack;

        PROCEDURE DoIt(arg: Integer);
        VAR
          bar, baz: Integer;
        BEGIN
          bar := arg;
          baz := bar;
        END;

        BEGIN
          DoIt(5);
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name

    maybe_offset = lambda x: f'{x:+}' if x else ''
    arg_offset = maybe_offset(symbols.children['DoIt']['arg'].storage.fp_offset)
    bar_offset = maybe_offset(symbols.children['DoIt']['bar'].storage.fp_offset)
    baz_offset = maybe_offset(symbols.children['DoIt']['baz'].storage.fp_offset)
    # Collect labels.
    label_scanner = LabelScanner(code)
    label_end_1 = label_scanner.nth_match('_BlockEnd_VarOnStack', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_VarOnStack|',   # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_VarOnStack:',    # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{sp}={sp}+2',            # Advance stack pointer to parameters.
         f'{sp}={sp}+1',            # Allocate space for the parameer.
         f'{s}[{fp}+2]=5',          # Push first parameter.
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         f'{sp}={sp}+2',            # Advance stack pointer for DoIt locals.
         'GOS |_SubEnter_VarOnStack_DoIt|',  # Call procedure.
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-5',            # Pop 1 arg, 2 locals, and maintenance.

         '_Exit_VarOnStack:',       # Program exit label.
         f'GO TO |{label_end_1}|',  # Jump to end of program.

         'REM /VarOnStack/DoIt',
         '_SubEnter_VarOnStack_DoIt:',  # DoIt subroutine implementation.
         f'{s}[{fp}{bar_offset}]={s}[{fp}{arg_offset}]',  # Assign arg to bar.
         f'{s}[{fp}{baz_offset}]={s}[{fp}{bar_offset}]',  # Assign bar to baz.
         '_Exit_VarOnStack_DoIt:',  # DoIt subroutine exit label.
         'RET',                     # Return to caller.

         f'{label_end_1}:',         # End of program.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # End of program.

  def test_program_05(self):
    """Can we compile references to stack values in nesting scopes?"""
    source = """\
        PROGRAM Nesty;
        VAR
          bar: Real;

        PROCEDURE DoIt(arg: Integer);
        VAR
          baz: Real;
        PROCEDURE Nest; BEGIN baz := arg; END;
        BEGIN
          Nest;
          bar := baz;
        END;

        BEGIN
          DoIt(5);
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name

    bar_name = symbols['bar'].storage.variable_name
    maybe_offset = lambda x: f'{x:+}' if x else ''
    arg_offset = maybe_offset(symbols.children['DoIt']['arg'].storage.fp_offset)
    baz_offset = maybe_offset(symbols.children['DoIt']['baz'].storage.fp_offset)
    # Collect labels.
    label_scanner = LabelScanner(code)
    label_end_1 = label_scanner.nth_match('_BlockEnd_Nesty_D', 1)
    label_end_2 = label_scanner.nth_match('_BlockEnd_Nesty_0', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_Nesty|',  # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_Nesty:',         # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{sp}={sp}+2',            # Advance stack pointer to "DoIt" params.
         f'{sp}={sp}+1',            # Allocate space for the parameter.
         f'{s}[{fp}+2]=5',          # Push first parameter
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         f'{sp}={sp}+1',            # Grow stack for "DoIt" local variable.
         'GOS |_SubEnter_Nesty_DoIt|',  # Call procedure "DoIt".
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-4',            # Pop everything off stack.

         '_Exit_Nesty:',            # Program exit label.
         f'DEL {bar_name}',         # Cleanup local variable.
         f'GO TO |{label_end_2}|',  # Jump to end of program.

         'REM /Nesty/DoIt',
         '_SubEnter_Nesty_DoIt:',   # "DoIt" subroutine implementation.
         f'{sp}={sp}+2',            # Advance stack pointer to "Nest" params.
         f'{s}[{fp}+2]={fp}',       # Push first parameter.
         f'{s}[{fp}+3]={fp}',       # Save old frame pointer for call.
         f'{fp}={fp}+4',            # Advance frame pointer for call.
         'GOS |_SubEnter_Nesty_DoIt_Nest|',  # Call procedure "Nest".
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-2',            # Pop everything off stack.
         f'{bar_name}={s}[{fp}{baz_offset}]',  # Assign baz to bar.
         '_Exit_Nesty_DoIt:',       # "DoIt" subroutine exit label.
         f'GO TO |{label_end_1}|',  # Jump to end of "DoIt".

         'REM /Nesty/DoIt/Nest',
         '_SubEnter_Nesty_DoIt_Nest:',  # "Nest" subroutine implementation.
         f'{s}[{s}[{fp}-1]{baz_offset}]={s}[{s}[{fp}-1]{arg_offset}]',
         '_Exit_Nesty_DoIt_Nest:',  # "Nest" subroutine exit label.
         'RET',                     # Return to caller.

         f'{label_end_1}:',         # End of "DoIt".
         'RET',                     # Return to caller.

         f'{label_end_2}:',         # End of program.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # That's all, folks!

  def test_program_06(self):
    """Can we compile a function that actually sets a return value?"""
    source = """\
        PROGRAM Returney;
        VAR
          bar: Integer;
        FUNCTION Five: Integer; BEGIN Five := bar; END;
        BEGIN
          bar := Five;
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(code)
    label_end_1 = label_scanner.nth_match('_BlockEnd_Returney', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_Returney|',   # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_Returney:',      # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{sp}={sp}+3',            # Advance stack pointer to parameters.
         f'{s}[{fp}+1]={fp}',       # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+3',            # Advance frame pointer for call.
         'GOS |_SubEnter_Returney_Five|',  # Call procedure.
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-2',            # Pop all but the function return value.
         f'{variable_name}={s}[{fp}]',  # Assign return value to bar.
         f'{sp}={sp}-1',            # Pop return value off the stack.

         '_Exit_Returney:',         # Program exit label.
         f'DEL {variable_name}',    # Cleanup local variable.
         f'GO TO |{label_end_1}|',  # Jump to end of program.

         'REM /Returney/Five',
         '_SubEnter_Returney_Five:',  # "Five" subroutine implementation.
         f'{s}[{fp}-3]={variable_name}',  # Assign 'bar' to the return value.
         '_Exit_Returney_Five:',    # "Five" subroutine exit label.
         'RET',                     # Return to caller.

         f'{label_end_1}:',         # Program exit point.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # That's all, folks!

  def test_program_07(self):
    """Can we compile a program that can't use native 4054 BASIC FOR?"""
    source = """\
        PROGRAM ForAgain;
        VAR
          bar: Integer;

        PROCEDURE DoIt;
        VAR
          i: Integer;  { Will be on the stack. }
        BEGIN
          FOR i := 1 TO 10 DO bar := i;
        END;

        BEGIN
          DoIt;
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(code)
    label_end_1 = label_scanner.nth_match('_BlockEnd_ForAgain', 1)
    label_fortop_1 = label_scanner.nth_match('_ForTop_ForAgain_DoIt', 1)
    label_forend_1 = label_scanner.nth_match('_ForEnd_ForAgain_DoIt', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_ForAgain|',   # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_ForAgain:',      # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{sp}={sp}+2',            # Advance stack pointer to "DoIt" params.
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         f'{sp}={sp}+1',            # Grow stack for "DoIt" local variable.
         'GOS |_SubEnter_ForAgain_DoIt|',  # Call procedure "DoIt".
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-3',            # Pop everything off stack.

         '_Exit_ForAgain:',         # Program exit label.
         f'DEL {variable_name}',    # Cleanup local variable.
         f'GO TO |{label_end_1}|',  # Jump to end of program.

         'REM /ForAgain/DoIt',
         '_SubEnter_ForAgain_DoIt:',  # "DoIt" subroutine implementation.
         f'{s}[{fp}]=1',            # For loop preparation: set initial value.
         f'{label_fortop_1}:',      # Top of the for loop.
         f'IF {s}[{fp}]>10 THE |{label_forend_1}|',   # Time to exit loop?
         f'{variable_name}={s}[{fp}]',  # Loop body: assign to bar.
         f'{s}[{fp}]={s}[{fp}]+1',  # Increment control variable.
         f'GO TO |{label_fortop_1}|',   # Jump to top of loop.
         f'{label_forend_1}:',      # Loop exit.
         '_Exit_ForAgain_DoIt:',    # "DoIt" subroutine exit label.
         'RET',                     # Return to caller.

         f'{label_end_1}:',         # Program exit point.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # That's all, folks!

  def test_program_08(self):
    """Can we compile a program with more complex enclosing scope access?"""
    # The correct behaviour that we're most interested in is marked below by
    # a (!) in a comment. There, B sets up the pointer to A's enclosing scope
    # (a.k.a. its "static link"), which is "DoIt".
    source = """\
        PROGRAM Scopey;

        PROCEDURE DoIt;
        VAR
          bar: Real;
          { A must write to a value in DoIt's stack frame, not B's. }
          PROCEDURE A; BEGIN bar := 123.45; END;
          PROCEDURE B; BEGIN A; END;
        BEGIN
          B;
        END;

        BEGIN
          DoIt;
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    # Collect labels.
    label_scanner = LabelScanner(code)
    label_end_1 = label_scanner.nth_match('_BlockEnd_Scopey_D', 1)
    label_end_2 = label_scanner.nth_match('_BlockEnd_Scopey_0', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_Scopey|',   # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_Scopey:',        # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{sp}={sp}+2',            # Advance stack pointer to "DoIt" params.
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         f'{sp}={sp}+1',            # Grow stack for "DoIt" local variable.
         'GOS |_SubEnter_Scopey_DoIt|',
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-3',            # Pop everything off stack.

         '_Exit_Scopey:',           # Program exit label.
         f'GO TO |{label_end_2}|',  # Jump to end of program.

         'REM /Scopey/DoIt',
         '_SubEnter_Scopey_DoIt:',  # "DoIt" subroutine implementation.
         f'{sp}={sp}+2',            # Advance stack pointer to "B" params.
         f'{s}[{fp}+1]={fp}',       # Save old frame pointer for call.
         f'{s}[{fp}+2]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+3',            # Advance frame pointer for call.
         'GOS |_SubEnter_Scopey_DoIt_B|',  # Call procedure "B".
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-2',            # Pop everything off stack.
         '_Exit_Scopey_DoIt:',      # "DoIt" subroutine exit label.
         f'GO TO |{label_end_1}|',  # Jump to "DoIt" subroutine exit.

         'REM /Scopey/DoIt/A',
         '_SubEnter_Scopey_DoIt_A:',  # "A" subroutine implementation.
         f'{s}[{s}[{fp}-1]]=123.45',  # Body: assing 123.45 to bar.
         '_Exit_Scopey_DoIt_A:',    # "A" subroutine exit label.
         'RET',                     # Return to caller.

         'REM /Scopey/DoIt/B',
         '_SubEnter_Scopey_DoIt_B:',  # "B" subroutine implementation.
         f'{sp}={sp}+2',            # Advance stack pointer to "A" params.
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={s}[{fp}-1]',  # (!) Enclosing context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         'GOS |_SubEnter_Scopey_DoIt_A|',  # Call procedure "A".
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-2',            # Pop everything off stack.
         '_Exit_Scopey_DoIt_B:',    # "B" subroutine exit label.
         'RET',                     # Return to caller.

         f'{label_end_1}:',         # "DoIt" subroutine exit.
         'RET',                     # Return to caller.

         f'{label_end_2}:',         # Program exit point.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # That's all, folks!

  def test_program_09(self):
    """Can we NOT compile subroutines that are never called?"""
    source = """\
        PROGRAM ExtraSubs;
        TYPE
          twobits = 0..3;
        VAR
          bar: twobits;

        PROCEDURE UnusedProc(a: twobits); BEGIN bar := a; END;

        PROCEDURE DoIt;
          FUNCTION UnusedFunc: Boolean;
          BEGIN
            UnusedFunc := bar > 2;
          END;
        BEGIN
          bar := 1;
        END;

        BEGIN
          DoIt;
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    variable_name = symbols['bar'].storage.variable_name
    # Collect labels.
    label_scanner = LabelScanner(code)
    label_end_1 = label_scanner.nth_match('_BlockEnd_ExtraSubs', 1)
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_ExtraSubs|',  # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_ExtraSubs:',     # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.

         f'{sp}={sp}+2',            # Advance stack pointer to "B" params.
         f'{s}[{fp}]={fp}',         # Save old frame pointer for call.
         f'{s}[{fp}+1]={fp}',       # Enclosing context for call.
         f'{fp}={fp}+2',            # Advance frame pointer for call.
         'GOS |_SubEnter_ExtraSubs_DoIt|',  # Call procedure "DoIt".
         f'{fp}={s}[{fp}-2]',       # Restore old frame pointer after call.
         f'{sp}={sp}-2',            # Pop everything off stack.

         '_Exit_ExtraSubs:',        # Program exit label.
         f'DEL {variable_name}',    # Cleanup local variable.
         f'GO TO |{label_end_1}|',  # Jump to end of program.

         'REM /ExtraSubs/DoIt',
         '_SubEnter_ExtraSubs_DoIt:',  # DoIt subroutine implementation.
         f'{variable_name}=1',         # Body: assign a value to bar.
         '_Exit_ExtraSubs_DoIt:',   # DoIt subroutine exit label.
         'RET',                     # Return to caller.

         f'{label_end_1}:',
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # That's all, folks!

  def test_program_10(self):
    """Can we compile a program that uses extensions?"""
    source = """\
        PROGRAM ExtensionUser;
        CONST
          neg = -1;
        VAR
          str: String[20];
          mx1, mx2: ARRAY [1..2, 1..4] OF Real;
          mx3: ARRAY [1..4, 1..2] OF Real;
          mx4: ARRAY [1..4, 1..4] OF Real;
        BEGIN
          WriteLn('Hello, world! ', Sin(Pi));
          Write('%12,34', 'far', '-out');
          WriteLn;
          InternalTapeParams(TRUE, FALSE, 3 > 4);
          SetGrads;
          Exit;
          Randomize;
          Call('EDITOR',123);
          NumToStr(64+971.2, str);
          Axis('@16', 20, 20);
          Axis(1, 2, 3, 4);
          Draw(123, 32.1);
          Draw('%42', 123, 32.1);
          MoveCursorGdu(12, 21);
          MoveCursorGdu('@42', 12, 21);
          Read(str);
          Read('@43,21', str);
          MatrixIdentity(mx1);
          MatrixInvert(mx1, mx2);
          MatrixInvert(mx1, mx1, mx2[2,3]);
          MatrixMultiply(mx3, mx2, mx4);
          MatrixTranspose(mx2, mx3);
          MatrixTranspose(mx4, mx4);
          DrawArrays(mx2, mx3);
          DrawArrays('@16', mx3, mx2);
          MatrixFill(mx2,  1.1, 2.2, 3.3, 4.4,
                          -neg, neg, 7.7, 8.8);
        END."""
    allocators_etc = AllocatorsEtc()
    ast, symbols, frame, quoted_constants = parse_and_allocate_symbols(
        source, allocators_etc)
    code = t4050_generator.program(
       ast, symbols, frame, quoted_constants, allocators_etc.static,
       allocators_etc.frame, allocators_etc.stack,
       allocators_etc.rev_call_graph)
    # Collect storage information.
    sp = frame.allocator.stack_pointer_resource.variable_name
    fp = frame.allocator.frame_pointer_resource.variable_name
    s = frame.allocator.stack_resource.variable_name
    str_name = symbols["str"].storage.variable_name
    mx1_name = symbols["mx1"].storage.variable_name
    mx2_name = symbols["mx2"].storage.variable_name
    mx3_name = symbols["mx3"].storage.variable_name
    mx4_name = symbols["mx4"].storage.variable_name
    # Another string we'll need:
    matrix_hash = f'{hash("1.1 2.2 3.3 4.4 1 -1 7.7 8.8") % 2**32:X}'
    # Now compare generated code against the "assembly" output we expect.
    self.assertEqual(
        [c.strip() for c in code],
        ['INC 10',                  # Preamble 1: line increment.
         'ORG 1',                   # Preamble 2: BASIC line 1
         'GO TO |_Program_ExtensionUser|',  # Preamble 3: Jump to origin.
         'ORG 100',                 # Preamble 4: Define origin.
         '_Program_ExtensionUser:',   # Preamble 5: Origin label.
         'INI',                     # Reset the interpreter.
         f'DIM {s}(100)',           # Allocate a 100-element stack.
         f'{sp}=1',                 # Set the stack pointer to 1.
         f'{fp}=1',                 # Set the frame pointer to 1.
         f'DIM {str_name}(20)',     # Allocate the string variable.
         f'DIM {mx1_name}(2,4)',    # Allocate matrix mx1.
         f'DIM {mx2_name}(2,4)',    # Allocate matrix mx2.
         f'DIM {mx3_name}(4,2)',    # Allocate matrix mx3.
         f'DIM {mx4_name}(4,4)',    # Allocate matrix mx4.
         'PRI "Hello, world! ";SIN(PI)',  # The WriteLn statement.
         'PRI %12,34:"far";"-out";',  # The Write statement.
         'PRI',                     # The second WriteLn statement.
         'PRI @33,0:1,0,(3>4)',     # The InternalTapeParams statement.
         'SET GRA',                 # The SetGrads statement.
         'GO TO |_Exit_ExtensionUser|',   # The Exit statement.
         f'{s}[{sp}]=RND(-1)',      # The Randomize statement.
         'CAL "EDITOR",123',        # The Call statement.
         f'{str_name}=STR (64+971.2)',  # The NumToStr statement.
         'AXI @16:20,20',           # The Axis statement.
         'AXI 1,2,3,4',             # The other Axis statement.
         'DRA 123,32.1',            # The Draw statement.
         'DRA %42:123,32.1',        # The other Draw statement.
         'PRI @32,21:12,21',        # The MoveCursorGdu statement.
         'PRI @42,21:12,21',        # The other MoveCursorGdu statement.
         f'INP {str_name}',         # The Read statement.
         f'INP @43,21:{str_name}',  # The other Read statement.
         f'CAL "IDN",{mx1_name}',   # The MatrixIdentity statement.
         f'{mx2_name}=INV {mx1_name}',   # The MatrixInvert statement.
         f'{mx1_name}=INV {mx1_name}',   # MatrixInvert statement 2, 1 of 2.
         f'{mx2_name}[2,3]=DET',    # MatrixInvert statement 2, 2 of 2.
         f'{mx4_name}={mx3_name} MPY {mx2_name}',  # MatrixMultiply statement.
         f'{mx3_name}=TRN {mx2_name}',   # The MatrixTranspose statement.
         f'{mx4_name}=TRN {mx4_name}',   # Second MatrixTranspose statement.
         f'DRA {mx2_name},{mx3_name}',   # The DrawArrays statement.
         f'DRA @16:{mx3_name},{mx2_name}',   # Second DrawArrays statement.
         f'_MatrixFill_{matrix_hash}:',  # MatrixFill statement label.
         'DAT 1.1,2.2,3.3,4.4,1,-1,7.7,8.8',   # MatrixFill statement data.
         f'RES |_MatrixFill_{matrix_hash}|',   # MatrixFill statement RESTORE.
         f'REA {mx2_name}',         # MatrixFill statement READ.
         '_Exit_ExtensionUser:',    # Program exit label.
         f'DEL {str_name}',         # Delete the string variable.
         f'DEL {mx1_name}',         # Delete matrix mx1.
         f'DEL {mx2_name}',         # Delete matrix mx2.
         f'DEL {mx3_name}',         # Delete matrix mx3.
         f'DEL {mx4_name}',         # Delete matrix mx4.
         f'DEL {s}',                # Delete the stack.
         f'DEL {sp}',               # Delete the stack pointer.
         f'DEL {fp}',               # Delete the frame pointer.
         'END'])                    # That's all, folks!


# And just in case you need to copy any of these statements while developing
# new tests:
    # import pprint;pprint.pprint(pascal_parser.asdict_rec(ast))
    # import pprint;pprint.pprint(code)
