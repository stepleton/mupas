#!/usr/bin/python3
"""An "assembler" for Tektronix 4050 BASIC.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

"Assembly" code for BASIC is pretty simple: it's basically a sequence of BASIC
statements that optionally use line labels instead of line numbers. Assembly
amounts to adding line numbers and substituting numbers in for labels in BASIC
statements.

The assembler introduces two new directives. Both take positive integer
arguments. `ORG` sets the line number of the next BASIC statement, and `INC`
sets the line number increment between adjacent BASIC statements. (These
directives are not case-sensitive.) The assembler also permits blank lines,
which are useful for code spacing and for defining line labels that refer to
code that follows the label.

The assembler has no "native" comments: use BASIC REM statements or (if you
have a -A computer like a 4052A) "tail comments" with the ! character. Tail
comments on ORG or INC directives will be discarded and can be used safely with
code bound for any 4050-series system (not just the -A machines).

Any line that starts with a non-whitespace character defines a line label. The
label is the string of alphanumeric-plus-underscore characters from the start
of the line to the first whitespace or `:` (colon) character. The first
character in a label must not be a numeral. **Labels are case-sensitive.**

A label will refer to the line number that would be assigned to a BASIC
statement that immediately follows the preceding assembly language line. Thus,
if INC is 10:

* A label appearing after a BASIC statement that will be assembled to line
  500 will indicate line 510.
* A label appearing after `ORG 600` will indicate line 600.
* Any label immediately preceding an ORG or INC directive will not be affected
  by that directive. This may be slightly surprising if the label and the
  directive share the same line.

Unless it's a directive, anything following a label definition and/or initial
whitespace is a BASIC statement, as far as this assembler is concerned. Within
BASIC statements, labels are delimited on both sides by percent-signs `|`, so
the label Foo would be invoked as `|Foo|`. This elaborate (ok, clunky) notation
facilitates use of a fairly primitive pattern-matching substitution scheme.
Labels inside comments will be still substituted, but the assembler does try to
avoid modifying string constants.

The assembler is not a macro assembler (yet).

**The assembler will not check that a "BASIC statement" is valid Tektronix 4050
BASIC.**

Assembly example: if | marks the beginning of a line, this "assembly code":

   |        ORG 101
   |        INC 5  ! Plenty of room for revisions
   |Start:
   |        LET A=0  ! Initialise A
   |
   |Loop    A=A+1
   |        PRINT A
   |        IF A<10 THEN |Loop|
   |        GO TO |Start|

will "assemble" to the following BASIC code:

   101 LET A=0  ! Initialise A
   106 A=A+1
   111 PRINT A
   116 IF A<10 THEN 106
   121 GO TO 101

The "assembled" BASIC program is a text file that uses carriage return
characters ($0D) as line delimiters, in keeping with the format used by the
Tektronix computers themselves.
"""

import argparse
import itertools
import re

from typing import Optional, Sequence


__version__ = 'Tektronix 4050 BASIC "assembler" 1.0 (circa January 2023)'


def _define_flags():
  """Defines an `ArgumentParser` for command-line flags used by this program."""
  flags = argparse.ArgumentParser(description=(
      '"Assemble" a non-line-numbered Tektronix 4050-series BASIC program.'))

  flags.add_argument('source', nargs='?', default='-',
                     help=('Source code to "assemble" --- see the program\'s '
                           'top-level docstring for details; omit to read '
                           'source from standard input'),
                     type=argparse.FileType('r'))

  flags.add_argument('-O', '--optimise',
                     default=False, action=argparse.BooleanOptionalAction,
                     help=('Perform optimisation on the source code before '
                           'assembling (fairly rudimentary for now)'),
                     type=bool)

  flags.add_argument('-o', '--output', default='-',
                     help=('Where to write the resulting "binary"; omit to '
                           'write to standard out'),
                     type=argparse.FileType('xb'))

  flags.add_argument('-v', '--version',
                     default=False, action=argparse.BooleanOptionalAction,
                     help='Print version and target option listing, then exit',
                     type=bool)

  return flags


def optimise(source: Sequence[str]) -> list[str]:
  """Perform "optimisations" on 4050-series BASIC "assembly" code.

  For now, optimisations primarily amount to simplifying runs of adjacent lines
  into single lines where possible. The optimiser also skips any blank lines in
  the input.

  Args:
    source: Unoptimised "assembly" code.

  Returns:
    "Optimised" "assembly" code.
  """
  # Our output will accumulate here.
  lines_out: list[str] = []

  # This optimiser can basically be in any of three states: optimising
  # runs of DEL statements, optimising runs of variable self-increments and
  # decrements (e.g. X=X+12), or not optimising anything and just passing
  # statements through. The optimiser recognises which states we're in by
  # whether these "accumulator lists" have any contents:
  lines_incs: list[re.Match[str]] = []
  lines_dels: list[re.Match[str]] = []

  # Regular expressions for matching the kind of code we can optimise.
  re_incs = re.compile(
      r"""(\s+)
          ([a-zA-Z]\w?)=  # Left-hand side variable and equal sign
          \2([+-]\d+)     # Repeat of variable and 'crement, with operator""",
      re.VERBOSE)
  re_dels = re.compile(
      r"""(\s+)           # Indentation, which we'll reproduce
          (?i:DEL)\w*\s+  # The DEL (or DELETE, or Dele, or...) keyword
          ([\w$,]+)       # Arguments for DEL""",
      re.VERBOSE)

  # Helpers that flush accumulated matches in our accumulator lists.
  def flush_lines_incs():
    if lines_incs:
      indent, var = lines_incs[0].groups()[:2]
      crement = sum(int(m.group(3)) for m in lines_incs)
      if crement:
        lines_out.append(f'{indent}{var}={var}{crement:+}')
      lines_incs[:] = []
  def flush_lines_dels():
    if lines_dels:
      indent = lines_dels[0].group(1)
      doomed = ','.join(m.group(2) for m in lines_dels)
      lines_out.append(f'{indent}DEL {doomed}')
      lines_dels[:] = []

  # Process each of the input lines.
  for line in source:
    # We've got an autoincrement. If we've been seeing autoincrements from
    # a different variable, then flush that accumulated information and start
    # accumulating anew. Otherwise keep accumulating.
    if match_incs := re_incs.fullmatch(line):
      if lines_incs and match_incs.group(2) != lines_incs[0].group(2):
        flush_lines_incs()
      lines_incs.append(match_incs)
    # We've got a deletion. Accumulate it.
    if match_dels := re_dels.fullmatch(line):
      lines_dels.append(match_dels)
    # If we've been accumulating autoincrements and we now have a line that
    # isn't an autoincrement, then flush what we've accumulated.
    if lines_incs:
      if not match_incs:
        flush_lines_incs()
    # If we've been accumulating deletions and we now have a line that isn't
    # a deletion, then flush what we've accumulated.
    if lines_dels:
      if not match_dels:
        flush_lines_dels()
    # And if we haven't got an autoincrement or a deletion, then just pass it
    # through.
    if not (match_incs or match_dels):
      lines_out.append(line)
  # Final flushes if we've reached the end of the code.
  flush_lines_incs()
  flush_lines_dels()

  return lines_out


def assemble(source: Sequence[str]) -> bytes:
  """Convert 4050-series BASIC "assembly" into a BASIC "executable".

  See module docstring for extensive documentation.

  Args:
    source: A list of lines of BASIC "assembly" program text.

  Returns:
    "Compiled" BASIC code, ready for use with 4050-series machines.
  """
  # pylint: disable=too-many-branches,too-many-nested-blocks  # A fair cop.
  # pylint: disable=too-many-statements  # This too I guess.

  # NOMENCLATURE NOTE: In this function, "num" is a variable that holds the
  # current line number of the source code input, and "line_number" refers to
  # the BASIC line number in the assembled source code.

  ### Step 1: Split source code into labels and statements. Empty strings mean
  # "no label" or "no statement". While we're here, check label validity.
  # We use regexes for both, and who knows, maybe they're even correct!
  labels_and_statements: list[tuple[Optional[str], Optional[str]]] = []
  split_regex = re.compile(  # How to split lines into labels and statements.
      r"""(?P<label>      # Our first capture group, called "label", is any...
            [^\s:]+       #   initial text which isn't whitespace or a colon.
          )?              # But it may not be present at all.
          (?:             # This next group is not a capture group.
            [:\s]         #   It's whitespace, optionally preceded by a colon.
            \s*           #   It goes for as long as the whitespace continues.
          )?              # But it may not be present at all.
          (?P<statement>  # Our second capture group, "statement": remaining...
            .*\S          #   text through the final non-whitespace character.
          )?              # But it may not be present at all.
          \s*             # Then we match any remaining whitespace.""",
      re.VERBOSE)
  label_regex = re.compile(  # What makes a valid ordinary label?
      r'[a-zA-Z_]\w*',  # Non-empty alphanumeric with a non-numeric first char.
      re.ASCII)         # No unicode.
  label_udk_regex = re.compile(  # What makes a valid User Defined Key label?
      r'@Key(\d+)',     # The string '@Key' followed by digits.
      re.ASCII)         # No unicode.

  for num, line in enumerate(source):
    # Parse labels and/or statements in the source line.
    matched = split_regex.fullmatch(line)  # Always matches.
    assert matched is not None             # So this is for mypy.
    label, statement = matched.groups()
    # If a label is defined here, check its validity.
    if label and not (
        label_regex.fullmatch(label) or label_udk_regex.fullmatch(label)):
      raise ValueError(f'Illegal label on line {num}: "{label}"')
    labels_and_statements.append((label, statement))

  ### Step 2: Classic assembler "first pass": determine the BASIC line number
  # associated with each (nonempty) statement and each line label.
  label_to_line_number: dict[str, int] = {}
  line_numbers_and_statements: list[tuple[int, int, str]] = []
  numberer = _LineNumberer()

  for num, (label, statement) in enumerate(labels_and_statements):
    # Associating line numbers to labels is the easy part, since any label we
    # see now takes the line number carefully computed further on below.
    if label is not None:
      matched = label_udk_regex.fullmatch(label)
      if matched is not None:
        key_line_number = 4 * int(matched.group(1))
        line_numbers_and_statements.append(
            (num, key_line_number, f'GO TO |{label}|'))
      label_to_line_number[label] = numberer.next_line_number

    # Determining the next label may depend on whether there is an ORG or INC
    # directive in the next line.
    if statement is not None:
      line_number, is_directive = numberer.process(num, statement)
      if not is_directive:
        line_numbers_and_statements.append((num, line_number, statement))

  ### Step 3: Part of the classic assembler "second pass": substitute line
  # numbers for labels.
  line_numbers_and_final_statements: list[tuple[int, str]] = []
  quote_regex = re.compile(  # For extracting string constants from lines.
      r'(?:[^"]+|"[^"]*")')  # "-free runs, or runs surrounded by ".
  label_regex = re.compile(  # For extracting label invocations from line parts.
      r'(?:[^\|]+|\|[@\w]+\|)')   # Runs surrounded by |, or anything else.

  for num, line_number, statement in line_numbers_and_statements:
    if statement.count('"') % 2:
      raise ValueError(
          f"Unterminated string constant on line {num}: '{statement}'")
    parts = quote_regex.findall(statement)  # Break statement into "parts".
    final_parts: list[str] = []  # Processed parts to accumulate in here.

    for part in parts:
      if part.startswith('"'):    # Parts that are string constants we...
        final_parts.append(part)  # pass through without processing.
      else:  # Other parts we scan for potential label substitution.
        if part.count('|') % 2:
          raise ValueError(
              f'Unterminated label invocation on line {num}: "{part}"')
        subparts = label_regex.findall(part)  # Break part into "subparts".
        final_subparts: list[str] = []  # Same accumulation gimmick here.

        for subpart in subparts:
          if subpart.startswith('|'):  # Process label invocations.
            label = subpart[1:-1]
            try:
              final_subparts.append(str(label_to_line_number[label]))
            except KeyError:
              raise ValueError(
                  f'Invocation of undefined label "{label}" on line {num}: '
                  f'{statement}') from None
          else:  # Or just pass through parts that aren't label invocations.
            final_subparts.append(subpart)

        final_part = ''.join(final_subparts)
        final_parts.append(''.join(final_part))

    final_statement = ''.join(final_parts)
    line_numbers_and_final_statements.append((line_number, final_statement))

  ### Step 4: The rest of the classic assembler "second pass": generate code.
  line_numbers_and_final_statements.sort()  # Sort lines by line number.
  code_parts: list[bytes] = [
    f'{line_number} {statement}'.encode('ASCII')
    for line_number, statement in line_numbers_and_final_statements]
  return b'\r'.join(code_parts) + b'\r\r'  # Two trailing CRs seems customary.


class _LineNumberer:
  """State and code for determining BASIC line numbers.

  Pass each statement (or directive as the case may be) in order into the
  `process` method. A `_LineNumberer` will keep track of the line number to
  give to successive lines of the program.

  Public read-only properties:
    next_line_number: Line number due to be assigned to the next BASIC
        statement.
    increment: Current line number increment between successive BASIC
        statements.
  """
  next_line_number: int
  increment: int
  # Did we just evaluate an ORG directive (or just start numbering for the
  # first time, which is equivalent to saying ORG 10)? Needed to avoid an INC
  # directive altering the code position immediately after an ORG directive.
  _just_orged: bool

  def __init__(self):
    """Initialise a _LineNumberer."""
    self.next_line_number = 10
    self.increment = 10
    self._just_orged = True  # All programs start with an implicit `ORG 10`.

  def process(self, num: int, statement: str) -> tuple[int, bool]:
    """Process a statement (or directive).

    Analyse the non-label part of a source code line, determining how to number
    that line (if it's a BASIC statement) and how to update the line number for
    the next source code line.

    Args:
      num: Source code line number associated with `statement`. Used only for
          error messages.
      statement: Non-empty source code line (sans label definition).

    Returns:
      a 2-tuple with these elements:
      [0]: The line number to assign to `statement`, if indeed it was a
          a statement. If it was a directive, you can ignore this number unless
          it's useful to you for some other reason.
      [1]: True if `statement` was a statement; false if a directive.
    """
    # We know we'll return the line number we computed in the last call to
    # this method.
    line_number_to_return = self.next_line_number

    # Computing the next line number is much more involved. If this is an ORG
    # or INC directive, handle it. Note trimming of tail comments.
    parts = list(itertools.takewhile(
        lambda x: not x.startswith('!'), statement.split()))
    if parts[0].upper() in ('ORG', 'INC'):
      # Try to parse the number in this directive.
      try:
        amount = int(parts[1])
        if amount < 1:
          raise ValueError
      except (ValueError, IndexError):
        raise ValueError(
            f'Illegal or missing numeric value in {parts[0].upper()} directive '
            f'on line {num} "{statement}"') from None
      # The number parses well, so handle the directive.
      if parts[0].upper() == 'ORG':
        self.next_line_number = amount
        self._just_orged = True
      elif parts[0].upper() == 'INC':
        # Adjust next line number to account for the new increment.
        if not self._just_orged:  # NOTE: INC does not reset _just_orged.
          self.next_line_number += (amount - self.increment)
        self.increment = amount  # Then update the increment.

      return line_number_to_return, True   # True: this was a directive.

    # This was not an ORG or INC directive, so calculate the next line number.
    else:
      self.next_line_number += self.increment
      self._just_orged = False
      return line_number_to_return, False  # False: this was not a directive.


def main(FLAGS: argparse.Namespace):
  """For when the assembler is run as a standalone executable."""
  if FLAGS.version:
    print(__version__)
    return

  # Read in source, assemble, and write to the output. We're not doing this
  # in a way that uses memory efficiently, which is fine.
  code = FLAGS.source.read().splitlines()
  if FLAGS.optimise:
    code = optimise(code)
  FLAGS.output.write(assemble(code))


if __name__ == '__main__':
  defined_flags = _define_flags()
  parsed_flags = defined_flags.parse_args()
  main(parsed_flags)
