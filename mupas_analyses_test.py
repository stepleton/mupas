"""Tests for the mupas_analyses module.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.
"""

import unittest

import mupas_analyses
import mupas_scopes
import preprocessor
import pascal_parser
import t4050_resources


def parse_and_get_quoted_constants(source_text: str) -> pascal_parser.AstNode:
  """Convert source text into an abstract syntax tree."""
  pp_source, quoted_constants, _ = preprocessor.preprocess(source_text)
  return pascal_parser.parse(pp_source), quoted_constants


class MupasAnalysesTest(unittest.TestCase):

  def test_get_symbols(self):
    """get_symbols() is capable of retrieving symbols."""

    source_text = """\
        PROGRAM Kazoo;
        CONST
          kConst1 = 'Have you ever heard a kazoo symphony?';
          kConst2 = -32767;
        TYPE
          Weekday = (Monday, Tuesday, Wednesday, Thursday, Friday);
        VAR
          var1: ARRAY [1..3] OF Weekday;
          var2: Integer;
          var3: Real;

        PROCEDURE Proc(parg1: Real; parg2: Weekday);
        CONST
          kPConst = 3.141;
        TYPE
          Spiciness = (Mild, Hot, Extreme);
        VAR
          pvar1: Spiciness;
          pvar2: Real;
        BEGIN
          WriteLn('Kazoo: instrument of kings.');
        END;

        FUNCTION Func(farg1: Integer; farg2: Weekday): Real;
        CONST
          kFConst = 'Membranophone.';
        VAR
          fvar: Weekday;
        BEGIN
          fvar := Tuesday;
          Func := 3.141 * fVar
        END;

        BEGIN
          Proc(Func(32, Friday), Thursday)
        END."""

    ast, quoted_constants = parse_and_get_quoted_constants(source_text)
    scope = mupas_analyses.get_symbols(ast, quoted_constants)

    # Do we have the nested scopes we expect? If so, extract them.
    self.assertEqual({'Proc', 'Func'}, set(scope.children))
    scope_proc = scope.children['Proc']
    scope_func = scope.children['Func']
    self.assertEqual(scope_proc.parent, scope)  # Check heredity.
    self.assertEqual(scope_func.parent, scope)

    # Make sure scopes have the right pathnames.
    self.assertEqual(scope.path, '/Kazoo')
    self.assertEqual(scope_proc.path, '/Kazoo/Proc')
    self.assertEqual(scope_func.path, '/Kazoo/Func')

    # Do we have the bindings we expect? This is a somewhat weak test, since we
    # are only checking for the type of symbol and not other useful metadata.
    # The call graph routines rely on more of this metadata and so (for now)
    # test that indirectly.
    self.assertEqual({'kConst1': mupas_scopes.ConstantStringSymbol,
                      'kConst2': mupas_scopes.ConstantIntegerSymbol,
                      'Monday': mupas_scopes.ConstantIntegerSymbol,
                      'Tuesday': mupas_scopes.ConstantIntegerSymbol,
                      'Wednesday': mupas_scopes.ConstantIntegerSymbol,
                      'Thursday': mupas_scopes.ConstantIntegerSymbol,
                      'Friday': mupas_scopes.ConstantIntegerSymbol,
                      'var1': mupas_scopes.VariableSymbol,
                      'var2': mupas_scopes.VariableSymbol,
                      'var3': mupas_scopes.VariableSymbol,
                      'Proc': mupas_scopes.SubroutineSymbol,
                      'Func': mupas_scopes.SubroutineSymbol},
                     {k: type(v) for k, v in scope.bindings.items()})
    self.assertEqual({'parg1': mupas_scopes.VariableSymbol,
                      'parg2': mupas_scopes.VariableSymbol,
                      'kPConst': mupas_scopes.ConstantRealSymbol,
                      'Mild': mupas_scopes.ConstantIntegerSymbol,
                      'Hot': mupas_scopes.ConstantIntegerSymbol,
                      'Extreme': mupas_scopes.ConstantIntegerSymbol,
                      'pvar1': mupas_scopes.VariableSymbol,
                      'pvar2': mupas_scopes.VariableSymbol},
                     {k: type(v) for k, v in scope_proc.bindings.items()})
    self.assertEqual({'farg1': mupas_scopes.VariableSymbol,
                      'farg2': mupas_scopes.VariableSymbol,
                      'kFConst': mupas_scopes.ConstantStringSymbol,
                      'fvar': mupas_scopes.VariableSymbol},
                     {k: type(v) for k, v in scope_func.bindings.items()})

    # Check that various constants have the values they should
    self.assertEqual(scope['kConst2'].value, -32767)
    self.assertEqual(scope_proc['kPConst'].value, 3.141)
    self.assertEqual(scope_proc['Extreme'].value, 2)

  def test_call_graphs(self):
    """Try get_call_graph() and get_reverse_transitive_call_graph()."""

    source_text = """\
        PROGRAM Goose;

        PROCEDURE Ralph;
        BEGIN
        END;

        PROCEDURE Frank;
        BEGIN
          Ralph;
        END;

        PROCEDURE Larry;
        BEGIN
          Ralph;
        END;

        PROCEDURE Brady;
        BEGIN
          Frank;
        END;

        PROCEDURE Steve;
        BEGIN
        END;

        PROCEDURE Kevin;
          PROCEDURE Aaron;
          BEGIN
            Ralph;
          END;

          PROCEDURE Marty;
          BEGIN
            Steve;
          END;
        BEGIN
          Aaron;
          Marty;
        END;

        FUNCTION Roger: Integer;
        BEGIN
          Steve;
          Roger := 5;  { Make sure we don't think this is a call. }
        END;

        PROCEDURE Billy;
        BEGIN
          Billy;  { Simple recursion. }
        END;

        PROCEDURE Ernie;
        BEGIN
          Brent;  { Mutual recursion. }
        END;

        PROCEDURE Brent;
        BEGIN
          Ernie;
        END;

        BEGIN
          Brady;
          Kevin;
          Billy;
          Ernie;
        END."""

    ast, quoted_constants = parse_and_get_quoted_constants(source_text)
    scope = mupas_analyses.get_symbols(ast, quoted_constants)

    call_graph = mupas_analyses.get_call_graph(ast, scope)
    self.assertEqual(
        {'/Goose': {'/Goose/Brady', '/Goose/Kevin', '/Goose/Billy', '/Goose/Ernie'},
         '/Goose/Ralph': set(),
         '/Goose/Frank': {'/Goose/Ralph'},
         '/Goose/Brady': {'/Goose/Frank'},
         '/Goose/Steve': set(),
         '/Goose/Kevin': {'/Goose/Kevin/Aaron', '/Goose/Kevin/Marty'},
         '/Goose/Kevin/Aaron': {'/Goose/Ralph'},
         '/Goose/Kevin/Marty': {'/Goose/Steve'},
         '/Goose/Billy': {'/Goose/Billy'},
         '/Goose/Ernie': {'/Goose/Brent'},
         '/Goose/Brent': {'/Goose/Ernie'}}, call_graph)

    rtc_graph = mupas_analyses.get_reverse_transitive_call_graph(call_graph)
    self.assertEqual(
        {'/Goose': set(),
         '/Goose/Ralph': {'/Goose',
                          '/Goose/Frank',
                          '/Goose/Brady',
                          '/Goose/Kevin',
                          '/Goose/Kevin/Aaron'},
         '/Goose/Frank': {'/Goose',
                          '/Goose/Brady'},
         '/Goose/Brady': {'/Goose'},
         '/Goose/Steve': {'/Goose',
                          '/Goose/Kevin',
                          '/Goose/Kevin/Marty'},
         '/Goose/Kevin': {'/Goose'},
         '/Goose/Kevin/Aaron': {'/Goose',
                                '/Goose/Kevin'},
         '/Goose/Kevin/Marty': {'/Goose',
                                '/Goose/Kevin'},
         '/Goose/Billy': {'/Goose',
                          '/Goose/Billy'},
         '/Goose/Ernie': {'/Goose',
                          '/Goose/Brent',
                          '/Goose/Ernie'},
         '/Goose/Brent': {'/Goose',
                          '/Goose/Brent',
                          '/Goose/Ernie'}}, rtc_graph)

  def test_add_static_resources_no_recursion_with_arrays_strings(self):
    """add_static_resources fails for recursion+array/string vars."""

    source_text = """\
        PROGRAM Failo;

        PROCEDURE Aarhus;
        VAR
          mystr : String[15];
        BEGIN
        END;

        { Belgrade and Carthage call themselves recursively, which means that
          they shouldn't be allowed to have string or array variables. }
        PROCEDURE Belgrade;
        CONST
          myconst = 'Hello there';
        VAR
          mystr : String[30];
        BEGIN
          Carthage;
        END;

        PROCEDURE Carthage;
        VAR
          myarray : Array[1..5] OF Integer;
        BEGIN
          Belgrade;
        END;

        BEGIN
          Aarhus;
          Belgrade;
        END."""

    # Parsing and preparing for the allocator.
    ast, quoted_constants = parse_and_get_quoted_constants(source_text)
    scope = mupas_analyses.get_symbols(ast, quoted_constants)
    call_graph = mupas_analyses.get_call_graph(ast, scope)
    rev_call_graph = mupas_analyses.get_reverse_transitive_call_graph(
        call_graph)
    allocator = t4050_resources.StaticAllocator()

    # The allocator should fail since it sees string or array variables in
    # functions that call each other recursively.
    with self.assertRaisesRegex(RuntimeError, 'recursively.*(mystr|myarray)'):
      mupas_analyses.add_static_resources(scope, rev_call_graph, allocator)

  def test_add_static_resources_constants(self):
    """add_static_resources allocates resources for constants."""

    source_text = """\
        PROGRAM Proggy;

        CONST
          numbery = 3.141;
          stringy = 'pi';

        { Albania and Belgium call each other recursively, but it's OK for
          them to have string constants (just not string variables). }
        PROCEDURE Albania;
        CONST
          capital = 'Tirana';
        BEGIN
          Belgium;
        END;

        FUNCTION Belgium : Integer;
        CONST
          capital = 'Brussels';
        BEGIN
          Belgium := 32;
          Albania;
        END;

        BEGIN
          Albania;
        END."""

    # Parsing and preparing for the allocator.
    ast, quoted_constants = parse_and_get_quoted_constants(source_text)
    scope = mupas_analyses.get_symbols(ast, quoted_constants)
    call_graph = mupas_analyses.get_call_graph(ast, scope)
    rev_call_graph = mupas_analyses.get_reverse_transitive_call_graph(
        call_graph)
    allocator = t4050_resources.StaticAllocator()

    # Running the allocator, which only needs to allocate space for constants.
    mupas_analyses.add_static_resources(
        scope, rev_call_graph, allocator)

    # There are three constant strings, and the allocator should have chosen
    # different BASIC string variables for each of them.
    proggy_stringy = scope.get_scope('/Proggy').bindings['stringy']
    albania_capital = scope.get_scope('/Proggy/Albania').bindings['capital']
    belgium_capital = scope.get_scope('/Proggy/Belgium').bindings['capital']

    registers = {b.storage.variable_name
                 for b in [proggy_stringy, albania_capital, belgium_capital]}

    self.assertEqual(len(registers), 3)  # That's three registers.
    self.assertTrue(all(r[-1] == '$' for r in registers))  # All for strings.

  def test_add_static_resources_variables(self):
    """add_static_resources allocates resources for certain variables."""

    source_text = """\
        PROGRAM Prog;

        CONST
          adit = 'I will claim a register thanks';

        VAR
          bevy: String[15];
          cyst: ARRAY [1..3, 2..5] OF Integer;
          duly: (One, Two, Three);

        { Ibex calls Ecru, so they must not share storage resources. }
        PROCEDURE Ecru(fief: Integer);
        VAR
          gamy: String[20];
          hiss: ARRAY [1..5] OF Real;
        BEGIN
        END;

        PROCEDURE Ibex;
        VAR
          jute: String[15];
        BEGIN
          Ecru(jute[3]);
        END;

        { Kith can recycle a storage resource that Ibex or Ecru uses. }
        PROCEDURE Kith;
        VAR
          lope: ARRAY [12345..12346] OF Char;
          mete: Integer;  { Should NOT get a register. }
        BEGIN
        END;

        { Mete is never called and should not be part of the analysis. }
        PROCEDURE Nous;
        VAR
          odor: String[123];
        BEGIN
        END;

        BEGIN
          Ecru(cyst[2,3]);
          Ibex;
          Kith;
        END."""

    # Parsing and preparing for the allocator.
    ast, quoted_constants = parse_and_get_quoted_constants(source_text)
    scope = mupas_analyses.get_symbols(ast, quoted_constants)
    call_graph = mupas_analyses.get_call_graph(ast, scope)
    rev_call_graph = mupas_analyses.get_reverse_transitive_call_graph(
        call_graph)
    allocator = t4050_resources.StaticAllocator()

    # Running the allocator, now allocating space for constants and variables.
    mupas_analyses.add_static_resources(scope, rev_call_graph, allocator)

    # Get binding information for strings and arrays in the program.
    adit = scope.get_scope('/Prog').bindings['adit']
    bevy = scope.get_scope('/Prog').bindings['bevy']
    cyst = scope.get_scope('/Prog').bindings['cyst']
    duly = scope.get_scope('/Prog').bindings['duly']
    gamy = scope.get_scope('/Prog/Ecru').bindings['gamy']
    hiss = scope.get_scope('/Prog/Ecru').bindings['hiss']
    jute = scope.get_scope('/Prog/Ibex').bindings['jute']
    lope = scope.get_scope('/Prog/Kith').bindings['lope']
    mete = scope.get_scope('/Prog/Kith').bindings['mete']
    odor = scope.get_scope('/Prog/Nous').bindings['odor']

    # We should not have allocated storage for mete in Kith, which is a scalar,
    # or odor in Nous, which is uncalled.
    self.assertIsNone(mete.storage)
    self.assertIsNone(odor.storage)

    # But the rest should all have registers assigned.
    adit_r, bevy_r, cyst_r, duly_r, gamy_r, hiss_r, jute_r, lope_r = (
        b.storage.variable_name
        for b in [adit, bevy, cyst, duly, gamy, hiss, jute, lope])

    # All four program-level constants/variables have unique registers.
    self.assertEqual(len(set((adit_r, bevy_r, cyst_r, duly_r))), 4)

    # Ecru and Ibex use three registers.
    self.assertEqual(len(set((gamy_r, hiss_r, jute_r))), 3)

    # Ecru and Ibex's registers don't overlap with the main program's.
    self.assertFalse(set((adit_r, bevy_r, cyst_r, duly_r)) &
                     set((gamy_r, hiss_r, jute_r)))

    # Kith can reuse registers used by Ecru and Ibex.
    self.assertIn(lope_r, set((gamy_r, hiss_r, jute_r)))

  def test_add_stack_resources(self):
    """add_stack_resources allocates resources for certain vars, parameters."""

    source_text = """\
        PROGRAM Prg;

        TYPE
          Awl = (boa, coy, din);

        VAR  { Global variables never use the stack. }
          eke: Integer;
          fie: String[10];

        FUNCTION Gym(hie: Boolean; VAR ire: Awl; jut: Awl): Real;
        BEGIN
          IF hie THEN Gym := ire + 3.14 ELSE Gym := jut + 2.72;
        END;

        PROCEDURE Kit(lox: Real);
        VAR
          ohm: ARRAY [1..3] OF Awl;
          pox: Char;

          FUNCTION Mar: Char;
          VAR
            nay: Boolean;
          BEGIN
            nay := lox > 1.62;
            Mar := nay;
          END;

        BEGIN
          pox := Mar;
        END;

        BEGIN
          Kit(1.41);
        END."""

    # Parsing and preparing for the allocators
    ast, quoted_constants = parse_and_get_quoted_constants(source_text)
    scope = mupas_analyses.get_symbols(ast, quoted_constants)
    call_graph = mupas_analyses.get_call_graph(ast, scope)
    rev_call_graph = mupas_analyses.get_reverse_transitive_call_graph(
        call_graph)
    static_allocator = t4050_resources.StaticAllocator()
    stack_allocator = t4050_resources.StackAllocator(
        stack_size=100, static_allocator=static_allocator)
    frame_allocator = stack_allocator.allocate()

    # Run the allocators.
    mupas_analyses.add_static_resources(scope, rev_call_graph, static_allocator)
    mupas_analyses.add_stack_resources(scope, frame_allocator)

    # Get binding information for variables and (non-reference) subroutine args.
    eke = scope.get_scope('/Prg').bindings['eke']
    fie = scope.get_scope('/Prg').bindings['fie']
    hie = scope.get_scope('/Prg/Gym').bindings['hie']
    jut = scope.get_scope('/Prg/Gym').bindings['jut']
    lox = scope.get_scope('/Prg/Kit').bindings['lox']
    ohm = scope.get_scope('/Prg/Kit').bindings['ohm']
    pox = scope.get_scope('/Prg/Kit').bindings['pox']
    nay = scope.get_scope('/Prg/Kit/Mar').bindings['nay']

    # Determine that they have been stored where we expect.
    self.assertIsInstance(eke.storage, t4050_resources.NumericVariable)
    self.assertIsInstance(fie.storage, t4050_resources.StringVariable)
    self.assertIsInstance(hie.storage, t4050_resources.StackValue)
    self.assertIsInstance(jut.storage, t4050_resources.StackValue)
    self.assertIsInstance(lox.storage, t4050_resources.StackValue)
    self.assertIsInstance(ohm.storage, t4050_resources.NumericVariable)
    self.assertIsInstance(pox.storage, t4050_resources.StackValue)
    self.assertIsInstance(nay.storage, t4050_resources.StackValue)

    # Determine that locations on the stack (relative to the frame pointer) are
    # as intended.
    self.assertEqual(hie.storage.fp_offset, 0)
    self.assertEqual(jut.storage.fp_offset, 1)
    self.assertEqual(lox.storage.fp_offset, 0)
    self.assertEqual(pox.storage.fp_offset, 1)
    self.assertEqual(nay.storage.fp_offset, 0)
