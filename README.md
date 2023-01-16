muPas: compile from a Pascal subset to Tektronix 4050-series BASIC
==================================================================

![A Koch curve fractal displayed on a Tektronix 4054A computer](
koch_curve.jpg "A Koch curve fractal displayed on a Tektronix 4054A computer")

muPas is:

- a barely-defined subset of the [Pascal programming language](
  https://en.wikipedia.org/wiki/Pascal_(programming_language)).
- a compiler (or perhaps a ["transpiler"](
  https://en.wikipedia.org/w/index.php?title=Transpiler) that converts programs
  written in muPas to a dialect of BASIC used by [Tektronix 4050-series](
  https://en.wikipedia.org/wiki/Tektronix_4050) vector storage tube graphical
  workstations.

The muPas compiler was written in a hurry so that its author, a BASIC-phobic
programmer who prefers GOTOs to remain in assembly language where they belong,
could enjoy using a Tektronix 4050-series computer without having to dirty his
hands with its relatively rudimentary BASIC. Given that muPas is in this sense
a means to an end, it lacks certain amenities and "polish". Some creature
comforts are absent, such as these:

- Testing has been fairly slight, so the compiler is surely full of bugs.
- Error messages are atrocious and don't tell you where to find offending code.

Additionally, limitations where muPas falls short of a "real" Pascal include:

- No support for sets, files, pointers, or record types.
- Subroutine parameters and return values cannot be anything besides scalar
  numbers, passed by value. (This limitation is relaxed somewhat for built-in
  callables called "extensions", which can pass by reference.)
- No support for libraries or units: the entire program must fit into a single
  compilation unit.

There are other limitations that you will discover when the compiler
cryptically refuses to compile your code.

The compiler allows programs to use the majority of built-in 4050-series BASIC
commands, as well as certain extended commands for A-model machines (4052A,
4054A), commands for the Option 30 dynamic graphics system, and commands for
the R12 graphics enhancement ROM.

:warning: **Features are likely to be added to muPas as the author attempts to
write increasingly complicated programs, and existing features and limitations
are liable to change as well.** :warning:


Documentation is limited
------------------------

muPas lacks good documentation, but at least the source code has a healthy
amount of commentary. What's most conspicuously missing is documentation about
the subroutine-like extensions that make up the "standard library" for the
Tektronix 4050-series BASIC target (all found in
[`t4050_stdlib_extensions.py`](t4050_stdlib_extensions.py)). These extensions
are what allow the muPas programmer to call 4050-series BASIC commands, and the
relationships between the muPas subroutine signatures and their BASIC
counterparts are not always obvious. Some understanding of how the extensions
are implemented may be necessary in order to deduce what the extensions do and
how they are meant to be called.


Requirements
------------

muPas requirements are fairly minimal: you'll need Python 3.10 or greater and a
recent version of the [Lark parsing toolkit](
https://github.com/lark-parser/lark). The version of Lark used to develop muPas
was 1.1.1.


Usage
-----

Invoke the compiler by executing [`mupas_compiler.py`](mupas_compiler.py). For
command-line option details and other help, run the compiler with the `-h`
flag. A typical invocation looks like this:

    mupas_compiler.py -O koch_curve.pas -o koch_curve.bas

[`koch_curve.pas`](koch_curve.pas) is an example program in this repository.
The resulting `koch_curve.bas` file is ready for use with a real Tektronix
4050-series computer or with an instance of the browser-based Tektronix 4051
emulator, like [this one](
https://jonbstanley.github.io/Tek405xEmulator/jsTEKTRONIX4051.html).


Touring muPas
-------------

For anyone wishing to understand muPas internals or to find more documentation
than what's available in this README, here is a "tour itinerary" through the
code:

- Start with [`mupas_compiler.py`](mupas_compiler.py), the top-level compiler
  program.

- The compiler generates "assembly code" that an assembler converts into native
  code for the target. For 4050-series BASIC, the assembler is in
  [`t4050_assembler.py`](t4050_assembler.py). Browse here to learn more about
  the kind of code that the 4050-series BASIC code generator is generating.

- Next, browse [`pascal_grammar.lark`](pascal_grammar.lark) and
  [`pascal_parser.py`](pascal_parser.py) to see the muPas parser. This parser
  is actually much more general than muPas: it uses a grammar for an extended
  version of Pascal that was designed in the early 1980s for the Apple Lisa
  computer. This Pascal includes early object-oriented extensions: the extended
  language was called [Clascal](https://en.wikipedia.org/wiki/Clascal) and was
  a direct ancestor to [Object Pascal](
  https://en.wikipedia.org/wiki/Object_Pascal) and [Delphi](
  https://en.wikipedia.org/wiki/Delphi_(software)). As object-oriented
  extensions to popular languages go, Clascal predated C++ and Objective-C by
  a year or so.

- The parser can't parse "pure" Pascal/Clascal source code: the code must be
  preprocessed first, which sets aside some nuisance constructs like comments
  and string literals. To see how this is done, and to see also how files can
  be included in other source code files (compare `#include` from C), view
  [`preprocessor_grammar.lark`](preprocessor_grammar.lark) and
  [`preprocessor.py`](preprocessor.py).

- When parsing completes, the compiler subjects the parsed code to various
  kinds of analysis in [`mupas_analyses.py`](mupas_analyses.py). The most
  important kinds of analysis identifies constant, variable, and subroutine
  symbols in the program, then determines where to store variables in memory.
  The analysis makes use of helpers in [`mupas_descent.py`](mupas_descent.py)
  to traverse the code.

- The storage analyses collect their bookkeeping in symbol scopes
  ([`mupas_scopes.py`](mupas_scopes.py)) which first match constants,
  variables, subroutine parameters, and function return values with their types
  ([`mupas_types.py`](mupas_types.py)). Subsequent analyses determine where
  some of these quantities are kept in memory: either in static memory
  resources like BASIC variables ([`mupas_static.py`](mupas_static.py)) or in
  storage locations on the stack ([`mupas_stack.py`](mupas_stack.py)). (In
  muPas programs, a BASIC array variable is allocated to serve as a [call
  stack](https://en.wikipedia.org/wiki/Call_stack).) It's worth noting that
  the stack in ([`mupas_stack.py`](mupas_stack.py)) is an abstraction: the
  real stack can have a different layout (and does for the 4050 BASIC target).
  See the docstring for details.

- Storage analyses are not written in an entirely target-independent way yet,
  but some of the 4050-series BASIC functionality does exist separately. In
  particular, information about how to allocate static resources and stack
  resources for the platform is managed by code in [`t4050_resources.py`](
  t4050_resources.py).

- With parsing and analysis complete, code generation can begin. For
  4050-series BASIC, code generation amounts to recursively creating and
  combining bits of compiled code stored in the data structures defined in
  [`t4050_compiled.py`](t4050_compiled.py). Much of this code generation takes
  place in [`t4050_generator.py`](t4050_generator.py), which recursively
  descends into the parse tree to carry out the creating and combining just
  mentioned.

- This said, although [`t4050_generator.py`](t4050_generator.py) generates code
  for fundamental structural, control-flow, and expression constructs, many
  additional muPas features --- even fundamental things like `WriteLn` and
  Boolean literals like `TRUE` --- take the form of procedure- and
  function-like elements called "extensions". Each invocation of an extension
  triggers the generation of a bit of extension-specific code, such as
  translating `WriteLn('Hello!');` in muPas to `PRINT "Hello!"` in BASIC.
  Dozens of extensions for the 4050-series BASIC target are defined in
  [`t4050_stdlib_extensions.py`](t4050_stdlib_extensions.py), implementing
  an interface defined in [`t4050_extensions.py`](t4050_extensions.py).


Nobody owns muPas
-----------------

The muPas compiler and any supporting programs, software libraries, and
documentation distributed alongside it are released into the public domain
without any warranty. See the [LICENSE](LICENSE) file for details.


Acknowledgements
----------------

It would not have been possible (or worthwhile) for me to write this muPas
compiler without the help of the following people and resources:

- [bitsavers.org](http://bitsavers.org)'s archived technical documentation.
- Monty McGraw's [archived Tektronix 4050-series programs and technical
  documentation](https://github.com/mmcgraw74/Tektronix-4051-4052-4054-Program-Files).
- Tektronix 4050-series technical documentation [at dvq.com](
  http://www.dvq.com/tektronix/).
- In-browser Tektronix 4051 emulator by Jon Stanley and others ([github](
  https://github.com/jonbstanley/Tek405xEmulator), [online emulator](
  https://jonbstanley.github.io/Tek405xEmulator/jsTEKTRONIX4051.html))
- The "Flash Drive" Tektronix 4924 tape drive emulator [[1]](
  https://github.com/Twilight-Logic/AR488_Store), [[2]](
  https://github.com/mmcgraw74/Tektronix-4050-GPIB-Flash-Drive), [[3]](
  https://forum.vcfed.org/index.php?threads/tektronix-4050-gpib-flash-drive-now-available.1238891/page-6#post-1281423)
  by Monty McGraw and John (?).
- J.L. for allowing me to borrow a Tektronix 4054A with Option 30!
- Help from John, Monty, and A.M. for help keeping the 4054A up-and-running.


-- _[Tom Stepleton](mailto:stepleton@gmail.com), 16 January 2023, London_
