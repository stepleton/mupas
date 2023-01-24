"""Tests for the t4050_assembler module.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.
"""

import unittest

import t4050_assembler


class T4050AssemblerTest(unittest.TestCase):
  """Test harness for testing the t4050_assembler module."""

  def test_basic(self):
    """Test the example program from the assembler's main docstring."""
    code = [
        '        ORG 101',
        '        INC 5  ! Plenty of room for revisions',
        'Start:',
        '        LET A=0  ! Initialise A',
        '',
        'Loop    A=A+1',
        '        PRINT A',
        '        PRINT %3,14:"Hi!"',
        '        IF A<10 THEN |Loop|',
        '        GO TO |Start|']
    result = t4050_assembler.assemble(code)
    self.assertEqual(result, '\r'.join([
        '101 LET A=0  ! Initialise A',
        '106 A=A+1',
        '111 PRINT A',
        '116 PRINT %3,14:"Hi!"',
        '121 IF A<10 THEN 106',
        '126 GO TO 101']).encode('ASCII') + b'\r\r')

  def test_errors(self):
    """Test various errors the assembler can throw."""
    with self.assertRaisesRegex(ValueError, 'Illegal label'):
      t4050_assembler.assemble(['bad-label:'])
    with self.assertRaisesRegex(ValueError, 'Unterminated string'):
      t4050_assembler.assemble(['   PRINT "hello"world"'])
    with self.assertRaisesRegex(ValueError, 'Unterminated label'):
      t4050_assembler.assemble(['   GO TO |nowhere'])
    with self.assertRaisesRegex(ValueError, 'Invocation of undefined'):
      t4050_assembler.assemble(['   GO TO |nowhere|'])
    with self.assertRaisesRegex(ValueError, 'numeric value in INC'):
      t4050_assembler.assemble(['   inc -1'])
    with self.assertRaisesRegex(ValueError, 'numeric value in INC'):
      t4050_assembler.assemble(['   Inc hams'])
    with self.assertRaisesRegex(ValueError, 'numeric value in ORG'):
      t4050_assembler.assemble(['   orG'])

  def test_org(self):
    """Test more nuanced behaviours of the ORG directive."""
    code = [  # ORG does not affect immediately-preceding labels.
        '       ORG 100',
        'label  ORG 200',
        '       GO TO |label|']
    result = t4050_assembler.assemble(code)
    self.assertEqual(result, b'200 GO TO 100\r\r')

  def test_inc(self):
    """Test more nuanced behaviours of the INC directive."""
    code = [
        '       INC 5',
        '       A=1+2',  # This statement should start on line 10.
        '       B=2+3',  # And this one on line 15.
        '       INC 4',
        '       C=3+4',  # But this one on line 19 and not 20.
        '       ORG 50',
        '       INC 10',
        '       D=4+5']  # And this one on line 50 and not 50 - 4 + 10 = 46
    result = t4050_assembler.assemble(code)
    self.assertEqual(result, '\r'.join([
        '10 A=1+2',
        '15 B=2+3',
        '19 C=3+4',
        '50 D=4+5']).encode('ASCII') + b'\r\r')

    code = [  # INC does not affect immediately-preceding labels.
        '       A=1+2',
        'label  INC 5',  # Dangerous!!
        '       GO TO |label|']
    result = t4050_assembler.assemble(code)
    self.assertEqual(result, '\r'.join([
        '10 A=1+2',
        '15 GO TO 20']).encode('ASCII') + b'\r\r')

  def test_forward_reference(self):
    """Test correct resolving of forward references."""
    code = [
        '       GO TO |ahead|',
        'ahead  A=1+2']
    result = t4050_assembler.assemble(code)
    self.assertEqual(result, b'10 GO TO 20\r20 A=1+2\r\r')

  def test_optimiser(self):
    """Test assembly code optimisation."""
    code = [
        '      A=A+2',
        '      A=A-9',
        '      B1=A+1',
        '      B1=B1-1',
        '      B1=B1-4',
        '      REM Hi',
        '      B1=B1-3',
        '      B1=B1+A',
        '      DEL A',
        '      DEL B1',
        '      Delicious I$',
        '      A+A+1',
        '      DEL A',
        '      A+A+1']
    result = t4050_assembler.optimise(code)
    self.assertEqual(result, [
        '      A=A-7',
        '      B1=A+1',
        '      B1=B1-5',
        '      REM Hi',
        '      B1=B1-3',
        '      B1=B1+A',
        '      DEL A,B1,I$',
        '      A+A+1',
        '      DEL A',
        '      A+A+1'])
