#!/usr/bin/python3
"""The muPas compiler.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This program presens "muPas" as a subset of the Pascal programming language.
For now, Pascal language features missing from muPas include:

   - Certain types: sets, files, pointers, records.
   - Subroutine parameters that are anything besides scalar numbers.
   - VAR subroutine parameters.
   - Units or libraries: the entire program must fit in one compilation unit.

Additional differences:

   - muPas variable and subroutine names are case-sensitive, as are the names
     of muPas extensions, which are usually how familiar Pascal fixtures like
     `Ord`, `WriteLn`, and `TRUE` and `FALSE` are implemented.

As muPas has no formal (or informal) specification, it's tempting to call this
program *the* muPas compiler, echoing such classics as "only perl can parse
Perl". But let's decide instead that "muPas" exists as a subject of ambivalent
social consensus, and as such anybody has as much authority as anyone else
pertaining to what might be muPas or what might be a muPas compiler. Perhaps a
particular sofa is a muPas compiler if enough people say so.

As implemented, this compiler makes use of a grammar that parses the Pascal
programming language supported by the Lisa Pascal Workshop for 1983's Apple
Lisa personal computer. (Obviously.) This grammar accommodates quite a few
language features that the rest of this compiler does not: besides the items
listed above, the grammar also supports Apple-invented extensions for
object-oriented programming (yielding a language known as "Clascal"). Isn't
that interesting?

Anyway, with considerable effort, you could extend this compiler to support
more targets than what it supports today. Your journey begins by adding a
target-specific subclass of `Compiler` and then adding it to the dict returned
by `_compiler_classes`.

But if all you want to know is how to run the compiler, just execute this
program with the -h flag.
"""

import abc
import argparse
import dataclasses
import functools
import sys

import mupas_analyses
import mupas_scopes
import mupas_stack
import mupas_static
import mupas_types
import preprocessor
import pascal_parser

from typing import Collection, Optional, Mapping, Sequence


__version__ = 'muPas compiler 0.1 circa January 2023'


def _define_flags():
  """Defines an `ArgumentParser` for command-line flags used by this program."""
  flags = argparse.ArgumentParser(
      description=(__version__),
      formatter_class=argparse.ArgumentDefaultsHelpFormatter)

  flags.add_argument('source', nargs='?', default='-',
                     help=('Program source code file to compile; omit to read '
                           'source from standard input'),
                     type=argparse.FileType('r'))

  flags.add_argument('-o', '--output', default='-',
                     help=('Where to write the resulting binary; omit to '
                           'write to standard out'),
                     metavar='FILENAME', type=argparse.FileType('wb'))

  flags.add_argument('-t', '--target', default='tek4050',
                     help='Compilation target: targets are described in -v',
                     choices=_compiler_classes().keys(),
                     type=str)

  flags.add_argument('-S', '--assembly-output', nargs='?', const=sys.stdout,
                     help=('Write compiled assembly code to a file, or if '
                           "'-', to standard output; assembling and binary "
                           'output is suppressed'),
                     metavar='FILENAME', type=argparse.FileType('w'))

  flags.add_argument('-O', '--optimise',
                     default=False, action=argparse.BooleanOptionalAction,
                     help='Enable optimisation', type=bool)

  flags.add_argument('-v', '--version',
                     default=False, action=argparse.BooleanOptionalAction,
                     help='Print version and target option listing, then exit',
                     type=bool)

  return flags


##########################
#### COMPILER CLASSES ####
##########################


@functools.cache
def _compiler_classes() -> Mapping[str, type['Compiler']]:
  """Construct mapping from target strings to Compiler subclasses.

  Keys in this dict are valid arguments to the -t flag.

  Returns:
    The mapping described.
  """
  return {
      'tek4050': Tektronix4050Compiler,
  }


class Compiler(abc.ABC):
  """Base class for a muPas compiler.

  Subclasses of this class will fill in the abstract methods for specific
  architectures.

  Note that the first line of Compiler subclass docstrings will be used
  in the target listing output for the -l flag.
  """

  @classmethod
  @abc.abstractmethod
  def define_flags(cls, flags: argparse.ArgumentParser):
    """Add target-specific flags to the compiler's command-line flags."""

  @classmethod
  @abc.abstractmethod
  def from_flags(cls, FLAGS: argparse.Namespace) -> 'Compiler':
    """Construct a target-specific Compiler configured by command-line flags."""

  def compile(
      self,
      source_text: str,
      filename: Optional[str] = None,
  ) -> Sequence[str]:
    """Compile a source text into a target-specific assembly format.

    Args:
      source_text: Complete source code for the program to compile.
      filename: Filename for the source text, or None if the text originated
          elsewhere. Used for error messages.

    Returns:
      Target-specifc assembly code for the compiled program.
    """
    # Preprocess. muPas does not support units right now.
    preprocessed_source_text, quoted_constants, units = (
        preprocessor.preprocess(source_text) if filename is None else
        preprocessor.preprocess(source_text, file_nest=(filename,)))
    if units: raise RuntimeError('muPas does not support units')

    # Parse the program.
    ast = pascal_parser.parse(preprocessed_source_text)
    if not isinstance(ast, pascal_parser.Program):
      where = f'in {filename}' if filename is not None else ''
      raise RuntimeError(f'muPas source {where} was not a program')

    # Check the parse tree for violations of muPas's simplifying assumptions.
    mupas_analyses.check_parse_tree(ast)

    # Build symbol table, call graph, and reverse transitive call graph.
    extra_symbols = self._extensions()
    symbols = mupas_analyses.get_symbols(ast, quoted_constants, extra_symbols)
    call_graph = mupas_analyses.get_call_graph(ast, symbols)
    rev_call_graph = (
        mupas_analyses.get_reverse_transitive_call_graph(call_graph))

    # Set up allocators and allocate resources for symbol table entries.
    static_allocator, stack_allocator, frame_allocator = self._allocators()
    mupas_analyses.add_static_resources(  # We always add static first.
        symbols, rev_call_graph, static_allocator)
    mupas_analyses.add_stack_resources(symbols, frame_allocator)

    # Generate and return assembly code.
    return self._generate(ast, symbols, quoted_constants, rev_call_graph,
                          static_allocator, stack_allocator, frame_allocator)

  @abc.abstractmethod
  def assemble(self, assembly: Sequence[str]) -> bytes:
    """Target-specific assembly for parsed muPas programs.

    Args:
      assembly: Target-specific assembly code emitted by `_generate`.

    Returns:
      Target-specifc binary code.
    """

  @abc.abstractmethod
  def _extensions(self) -> Mapping[str, mupas_scopes.ExtensionSymbol]:
    """Collect target-specific extensions.

    Extensions are symbols which are used in muPas programs like subroutine
    calls. During code generation, the code generator uses the extension symbol
    and any argument expressions to generate target-specific code. As an
    example, the Tektronix 4050 BASIC target uses extensions extensively to
    implement Pascal standard procedures and functions by invoking their BASIC
    analogues.

    Returns:
      A collection of extension symbols that will be added to the symbol table
      at top-level.
    """

  @abc.abstractmethod
  def _allocators(self) -> tuple[mupas_static.Allocator,
                                 mupas_stack.StackAllocator,
                                 mupas_stack.FrameAllocator]:
    """Create target-specific resource allocators.

    Allocators do bookkeeping for static and stack-based resources, abstractly
    speaking. (That means: while resources are allocated to what the compiler
    generically likes to think of as static and stack-based resources, the
    target architecture may not use those things at all --- allocators will
    simply present that abstraction and designate some other resource for the
    code generation step to put to use.)  The `add_..._resources` functions
    in mupas_analyses will use these allocators to allocate resources required
    by the muPas program.

    Returns:
      - [0] A static resources allocator.
      - [1] An allocator that allocates entire stacks.
      - [2] An allocator that allocates stack frames in stacks created by [1].
    """

  @abc.abstractmethod
  def _generate(
    self,
    ast: pascal_parser.Program,
    symbols: mupas_scopes.SymbolScope,
    quoted_constants: Sequence[str],
    rev_call_graph: Mapping[str, Collection[str]],
    static_allocator: mupas_static.Allocator,
    stack_allocator: mupas_stack.StackAllocator,
    frame_allocator: mupas_stack.FrameAllocator,
  ) -> Sequence[str]:
    """Target-specific code generation for parsed muPas programs.

    Args:
      ast: Abstract syntax tree for a muPas program.
      symbols: Symbol table for the program.
      quoted_constants: Quoted constants for the program.
      rev_call_graph: Reverse transitive call graph for the program, as
          generated by `mupas_analyses.get_reverse_transitive_call_graph`.
      static_allocator: The static resources allocator used to allocate the
          static resources in `symbols`.
      stack_allocator: The stack allocator used to allocate the stack used by
          stack-based resources in `symbols`.
      frame_allocator: The frame allocator used to allocate stack frames used
          by stack-based resources in `symbols`.

    Returns:
      Target-specific assembly code suitable for passing to the `assemble`
      method.
    """


@dataclasses.dataclass
class Tektronix4050Compiler(Compiler):
  """Tektronix 4050 BASIC

  This compiler subclass generates BASIC code for the Tektronix 4050 series
  of vector storage tube personal workstations.

  Attributes:
    stack_size: Allocate an array with this many elements for the stack.
    optimise: Whether to attempt available optimisations.
  """
  stack_size: int = 200
  optimise: bool = False
  origin: int = 100
  line_increment: int = 10
  line1_jump: bool = True

  @classmethod
  def define_flags(cls, flags: argparse.ArgumentParser):
    """Add Tek4050-specific flags to the compiler's command-line flags."""
    flags.add_argument('--t4050-stack-size', default=cls.stack_size,
                       help=('<tek4050> Allocate an array with this many '
                             'elements for the stack'),
                       metavar='STACK_SIZE', type=int)
    flags.add_argument('--t4050-origin', default=cls.origin,
                       help=("<tek4050> Line number for the compiled program's "
                             'first BASIC statement, not counting any '
                             'statement generated for --t4050-line1-jump'),
                       metavar='ORIGIN', type=int)
    flags.add_argument('--t4050-line-increment', default=cls.line_increment,
                       help=('<tek4050> Spacing between successive line '
                             'numbers in the compiled program'),
                       metavar='INCREMENT', type=int)
    flags.add_argument('--t4050-line1-jump', default=cls.line1_jump,
                       action=argparse.BooleanOptionalAction,
                       help=('<tek4050> Include a GO TO from BASIC line 1 to '
                             'the origin specified by --t4050-origin'),
                       type=bool)

  @classmethod
  def from_flags(cls, FLAGS: argparse.Namespace) -> Compiler:
    """Construct a Tektronix4050Compiler configured by command-line flags."""
    return cls(stack_size=FLAGS.t4050_stack_size, optimise=FLAGS.optimise,
               origin=FLAGS.t4050_origin,
               line_increment=FLAGS.t4050_line_increment,
               line1_jump=FLAGS.t4050_line1_jump)

  def assemble(self, assembly: Sequence[str]) -> bytes:
    """Tek4050-specific assembly for parsed muPas programs."""
    import t4050_assembler
    return t4050_assembler.assemble(assembly)

  def _extensions(self) -> Mapping[str, mupas_scopes.ExtensionSymbol]:
    """Collect Tek4050-specific extensions."""
    import t4050_stdlib_extensions
    return t4050_stdlib_extensions.extensions()

  def _allocators(self) -> tuple[mupas_static.Allocator,
                                 mupas_stack.StackAllocator,
                                 mupas_stack.FrameAllocator]:
    """Create Tek4050-specific resource allocators."""
    import t4050_resources
    static_allocator = t4050_resources.StaticAllocator()
    stack_allocator = t4050_resources.StackAllocator(
        self.stack_size, static_allocator)
    frame_allocator = stack_allocator.allocate()

    return static_allocator, stack_allocator, frame_allocator

  def _generate(
    self,
    ast: pascal_parser.Program,
    symbols: mupas_scopes.SymbolScope,
    quoted_constants: Sequence[str],
    rev_call_graph: Mapping[str, Collection[str]],
    static_allocator: mupas_static.Allocator,
    stack_allocator: mupas_stack.StackAllocator,
    frame_allocator: mupas_stack.FrameAllocator,
  ) -> Sequence[str]:
    """Tek4050-specific code generation for parsed muPas programs."""
    import t4050_resources
    assert isinstance(static_allocator, t4050_resources.StaticAllocator)
    assert isinstance(stack_allocator, t4050_resources.StackAllocator)
    assert isinstance(frame_allocator, t4050_resources.FrameAllocator)
    import t4050_assembler
    import t4050_generator

    # Create an enclosing stack frame for the program that mimics what you'd
    # have for a zero-parameter procedure with no local variables. This
    # doesn't actually take up any space in the final program; it's just for
    # interfacing with the rest of muPas's bookkeeping.
    frame_typeinfos = [mupas_types.CodePointer(),  # Return address.
                       mupas_types.DataPointer(),  # Old frame pointer.
                       mupas_types.DataPointer()]  # Enclosing scope pointer.
    frame = frame_allocator.allocate(frame_typeinfos)

    # Generate code and optimise if requested.
    assembly = t4050_generator.program(
        ast, symbols, frame, quoted_constants,
        static_allocator, frame_allocator, stack_allocator, rev_call_graph)
    if self.optimise: assembly = t4050_assembler.optimise(assembly)
    return assembly


######################
#### MAIN PROGRAM ####
######################


def main(FLAGS: argparse.Namespace):
  """For when the compiler is run as a standalone executable."""
  # Print version and target information and exit, if requested.
  if FLAGS.version:
    print(__version__)
    print('Targets available:')
    for target, cls in _compiler_classes().items():
      detail = cls.__doc__.splitlines()[0] if cls.__doc__ is not None else ''
      print(f'\t{target:16}{detail}')
    return

  # Prepare compiler object for the selected target.
  try:
    compiler = _compiler_classes()[FLAGS.target].from_flags(FLAGS)
  except KeyError:
    raise ValueError(f'Unrecognised target architecture {FLAGS.target}')

  # Compile and assemble.
  source_text = FLAGS.source.read()
  assembly = compiler.compile(source_text, FLAGS.source.name)
  if FLAGS.assembly_output is None:
    binary = compiler.assemble(assembly)
    FLAGS.output.write(binary)
  else:
    FLAGS.assembly_output.write('\n'.join(assembly) + '\n')


if __name__ == '__main__':
  # Define command-line flags, including target-specific ones.
  flags = _define_flags()
  for _, compiler_class in sorted(_compiler_classes().items()):
    compiler_class.define_flags(flags)
  # Parse flags and call main()
  FLAGS = flags.parse_args()
  main(FLAGS)
