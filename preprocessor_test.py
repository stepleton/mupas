"""Tests for preprocessor module.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.
"""

import textwrap
import unittest

import preprocessor


class PreprocessorTest(unittest.TestCase):
  """Test harness for testing the preprocessor module."""

  def test_basic_example(self):
    """Mainly serves to show basic preprocessor operation."""

    source_text = textwrap.dedent(
        """\
        (* Hello World program example *) PROGRAM Hello;
        USES
          {$U Beans.LIB} Beans;
        BEGIN
          WriteLn ('What''s up, world!');
        END.""")

    expected_preprocessed = textwrap.dedent(
        """\
                                          PROGRAM Hello;
        USES
                         Beans;
        BEGIN
          WriteLn (!id=0,len=20!);
        END.""")

    preprocessed, quoted_constants, units_mapping = preprocessor.preprocess(
        source_text)

    self.assertEqual(preprocessed, expected_preprocessed)
    self.assertEqual(quoted_constants, ["What's up, world!"])
    self.assertEqual(units_mapping, {'Beans.LIB': {'Beans'}})


  def test_include(self):
    """File inclusion works as expected."""

    source_outer_text = textwrap.dedent(
        """\
        {$U Aaa.LIB} Aaa;
        {$U Bbb.LIB} Bbb;
        'Hello' {$I include.text} 'there'""")

    source_inner_text = textwrap.dedent(
        """\
        {$U Aaa.LIB} NotAaa;
        {$U Ccc.LIB} Ccc;
        'Goodbye'""")

    expected_preprocessed = textwrap.dedent(
        """\
                     Aaa;
                     Bbb;
        !id=0,len=7!              NotAaa;
                     Ccc;
        !id=1,len=9! !id=2,len=7!""")

    def include_loader(filename):
      """An include loader that only returns source_inner_text."""
      self.assertEqual(filename, 'include.text')
      return source_inner_text

    preprocessed, quoted_constants, units_mapping = preprocessor.preprocess(
        source_outer_text, include_loader=include_loader)

    self.assertEqual(preprocessed, expected_preprocessed)
    self.assertEqual(quoted_constants, ['Hello', 'Goodbye', 'there'])
    self.assertEqual(units_mapping, {'Aaa.LIB': {'Aaa', 'NotAaa'},
                                     'Bbb.LIB': {'Bbb'},
                                     'Ccc.LIB': {'Ccc'}})


  def test_include_loop_detected(self):
    """The preprocessor detects when files include themselves."""

    def include_loader(filename):
      next_digit = (int(filename[-1]) + 1) % 6
      return f'Hello and (*$I file_{next_digit}*)'

    with self.assertRaisesRegex(RuntimeError, 'Found an include loop'):
      preprocessor.preprocess('Start {$I file_0}',
                              include_loader=include_loader)
