"""Lisa Pascal/Clascal preprocessor.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This preprocessor uses the grammar in `preprocessor_grammar.lark` to help
preprocess a Pascal/Clascal program text supplied in a (long) string. For more
details on what this entails, see the docstring for `preprocess()` in this
module.
"""

import dataclasses
import enum
import functools
import os
import re

import lark

from typing import Callable, Collection, Mapping, Optional, Sequence


def preprocess(
    program_text: str,
    *,
    file_nest: Sequence[str] = ('__unspecified_file__',),
    quoted_constants: Optional[Sequence[str]] = None,
    include_loader: Optional[Callable[[str], str]] = None,
) -> tuple[str, Sequence[str], Mapping[str, Collection[str]]]:
  r"""Preprocess a *ascal source file.

  In a nutshell, preprocessing a source file means stripping comments and
  extracting string and character constants, replacing the constants with
  special placeholders that the compiler will recognise.

  There are additional details: the Lisa Pascal compiler has a facility for
  "magical" comments that behave like preprocessor directives or configuration
  options for C compilers. Comments honoured by this preprocessor include:

  - {$I filename}: include the specified source file at this location.
  - {$U filename}: (UNITS clause only) Search the specified file for the unit
      named immediately after this comment.

  There are numerous other compiler commands not supported by (or relevant to)
  this preprocessor. See chapter 12 of the Lisa Pascal 3.0 Reference Manual for
  details.

  For "non-magical" comments, "stripping" means replacing the entire comment
  with ` ` (space) characters. This means that line and column numbering in
  error messages and the like will still work, if those are ever attempted. (We
  assume the user will prefer not to use tabs (\t) anywhere). Meanwhile, the
  placeholders for string constants look like this: `!id=123,len=20!`, where
  the number after `!id=` is an index into `quoted_constants` (locating the
  string that the placeholder is holding a place for), and the number after
  `len=` is the length of the string literal (including the delimiting `'`
  characters) in the source code. This too is meant to help facilitate column
  number calculatons.

  Args:
    program_text: Input program text.
    file_nest: A sequence of filenames whose last element is the name of the
        file that contained the data in `program_text`. Preceding elements
        describe a chain of nested {$I}-included files. This argument is used
        to check for include loops.
    quoted_constants: A list of string and character constants extracted so far
        from the file(s) that {$I}-include the program text. Only for use by
        recursive calls to this function; external callers should not supply
        this argument.
    include_loader: A function that loads program text from filenames
        specified in {$I} directives. The string argument to this function is
        the filename; it should return the file contents as a string.

  Returns:
    - [0] Preprocessed source text with comments stripped, strings replaced
      by placeholders, and included files included.
    - [1] List of string constants extracted from this file and any included
      files. Placeholders in the preprocessed source refer to elements in
      this list.
    - [2] Maps filenames specified in {$U} comments to the first program text
      word immediately following on the same line that could be a valid Lisa
      Pascal identifier. Will map to the empty string if no such string can
      be found.

  Raises:
    RuntimeError: a loop has been detected among nested {$I} directives.
    ValueError: An $U directive does not precede an identifier.
  """
  # Default argument handling.
  include_loader = include_loader or _default_include_loader
  quoted_constants_ = [] if quoted_constants is None else list(quoted_constants)
  del quoted_constants  # All to avoid type confusion.

  # Compile the program text into a sequence of Items.
  items = _Transformer().transform(_parser().parse(program_text))

  # This helper computes the string placeholder that replaces a string literal
  # in the preprocessed source code file. Note that we estimate the length of
  # the quoted literal from the string that the literal encoded --- in other
  # words, the Pascal equivalent of len(repr(literal)).
  def placeholder(id_number: int, string: str):
    source_len = len(string) + 2 + string.count("'")
    return f'!id={id_number},len={source_len}!'

  # Iterate over the items, accumulating preprocessed program text string
  # constants, and $U units-clause file specification details. File inclusion
  # calls this function recursively.
  preprocessed_text = []
  units_map: dict[str, set[str]] = {}
  empty_item = Item(Kind.PROGRAM_TEXT, '')
  for item, next_item in zip(items, items[1:] + [empty_item]):
    # Processing program text. We pass it through verbatim.
    if item.kind == Kind.PROGRAM_TEXT:
      preprocessed_text.append(item.value)

    # Replace quoted constants with placeholders and accumulate their values.
    elif item.kind == Kind.QUOTED_CONSTANT:
      id_number = len(quoted_constants_)
      preprocessed_text.append(placeholder(id_number, item.value))
      quoted_constants_.append(item.value)

    # Handle comments, including "magic" ones.
    elif item.kind == Kind.COMMENT:
      # Extract the text within the comment. This is guaranteed to match.
      comment_body = re.fullmatch(  # type: ignore
          r"""(?:{|\(\*)  # Either { or (*
              (.*)        # Whatever
              (?:}|\*\))  # Either } or )*""",
          item.value, re.X | re.DOTALL)[1]
      # See if this comment is one of the file-related "magic comments"
      magic_match = re.fullmatch(
          r"""\s*(\$I|\$U)\s*  # The $I include marker
              (\S*)      # The file to include
              \s*$       # Any trailing whitespace""",
          comment_body, re.X | re.IGNORECASE)

      # If the user wanted to include a file, include it.
      if magic_match and magic_match[1].upper() == '$I':
        filename = magic_match[2]
        # Avoid include loops.
        nested_file_nest = tuple(file_nest) + (filename,)
        if filename in file_nest: raise RuntimeError(
            f'Found an include loop: {"->".join(nested_file_nest)}')
        # Include the file by calling ourself recursively.
        nested_text, quoted_constants, nested_units_map = preprocess(
            include_loader(filename),
            file_nest=nested_file_nest,
            quoted_constants=quoted_constants_,
            include_loader=include_loader)
        quoted_constants_ = list(quoted_constants)  # Also for mypy...
        # Add included text and augment units map.
        preprocessed_text.append(nested_text)
        for nu_filename, nu_units in nested_units_map.items():
          units_map.setdefault(nu_filename, set()).update(nu_units)

      # Or if the user wanted to specify a unit file, then do that.
      elif magic_match and magic_match[1].upper() == '$U':
        filename = magic_match[2]
        if next_item.kind != Kind.PROGRAM_TEXT: raise ValueError(
          f'{item.value} was not followed by an identifier')
        unit_match = re.match(r'\s*([A-Za-z][0-9A-Za-z]*)?', next_item.value)
        assert unit_match is not None  # To reassure mypy.
        units_map.setdefault(filename, set()).add(unit_match[1] or '')
        # But retain the space used by the {$U} comment.
        preprocessed_text.append(re.sub(r'[^\t\r\n\f]', ' ', item.value))

      # Otherwise, just replace the entire comment text with blanks.
      else:
        preprocessed_text.append(re.sub(r'[^\t\r\n\f]', ' ', item.value))

  return ''.join(preprocessed_text), quoted_constants_, units_map


class Kind(enum.Enum):
  """Types of Item encountered during preprocessing."""
  PROGRAM_TEXT = 1     # Ordinary Pascal program text
  COMMENT = 2          # A {comment} or a (*comment*)
  QUOTED_CONSTANT = 3  # A 'quoted constant''s type'


@dataclasses.dataclass
class Item:
  """Container for source code elements.

  The preprocessing mechanism first compiles source code into a sequence of
  Items, which have a kind (see Kind) and a string value that is usually a
  gently processed copy of the source code text.
  """
  kind: Kind
  value: str

  def __str__(self) -> str: return self.value


class _Transformer(lark.Transformer):
  """A Lark Transformer that compiles a parse tree into a sequence of Items.

  The grammar in preprocessor_grammar.lark produces a parse tree for program
  text that's more complicated than we need: nested comments produce elaborate
  trees and so on. This Transformer collapses the tree into a flat list of
  Items containing Pascal program text, comments, or quoted constants.
  """

  start = lambda self, items: items

  def program_text(self, items):
    return Item(Kind.PROGRAM_TEXT, ''.join(items))

  def comment_curly_brace(self, items):
    return Item(Kind.COMMENT, '{' + ''.join(str(i) for i in items) + '}')

  def comment_paren_star(self, items):
    return Item(Kind.COMMENT, '(*' + ''.join(str(i) for i in items) + '*)')

  def quoted_constant(self, items):
    return Item(Kind.QUOTED_CONSTANT, ''.join(items))

  comment_curly_brace_text = lambda self, items: ''.join(items)
  comment_paren_star_text = lambda self, items: ''.join(items)
  quoted_constant_text = lambda self, items: ''.join(items)
  escaped_quote = lambda *_: "'"


@functools.cache
def _parser() -> lark.Lark:
  """Create/retrieve a singleton Lark parser from the preprocessor grammar."""
  module_dir = os.path.dirname(os.path.realpath(__file__))
  return lark.Lark.open(os.path.join(module_dir, 'preprocessor_grammar.lark'))


def _default_include_loader(filename: str) -> str:
  """A default, basic loader for files included with $I.

  The $I compiler command directs the compiler to begin reading source code
  from a specified file. The `preprocess` function loads entire source code
  files into RAM, but to accomplish this it makes use of a loader function
  that can be supplied by the caller. If the caller supplies nothing, then
  `preprocess` uses this default loader that simply uses basic Python file
  I/O.

  Args:
    filename: Name of a Pascal source code file to read.

  Returns:
    Contents of the file specified by `filename`.
  """
  with open(filename, 'r') as f: return f.read()
