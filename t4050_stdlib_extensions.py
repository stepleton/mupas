"""'Standard' muPas extensions for the Tektronix 4050-series BASIC target.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This large library of extensions attempts to (a) give the muPas programmer
access to most of the 4050-series BASIC's built-ins, and (b) offer minimal
stand-ins for various familiar facilities to the Pascal programmer, like
`WriteLn` and `Exit`. Remember that extension names are case-sensitive, so
unlike Pascal, `TRUE` can only be `TRUE` and `WriteLn` can only be `WriteLn`.

A number of extensions are hardware- or option-specific. The `Brightness`
and `CharSize` extensions only work on the Tektronix 4054 and 4054A machines,
for example. Additionally:

  - Extensions prefixed by `Rg` are for systems equipped with the Option 30
    Refresh Graphics device.
  - Extensions prefixed by `R12` are for systems equipped with the R12
    Graphics Enhancement ROM.
  - A number of extensions are present that support expanded BASIC facilities
    available on -A models (4052A, 4054A). These have no distinguishing
    prefix.

It's your program's job to determine whether it can function correctly on
whatever hardware it happens to be running on.

While some names for BASIC-language facilities are also used for the names of
corresponding extensions, others have been changed to meet practical needs or
to suit the author's own preferences. `SIN` from BASIC is now `Sin`, for
example, but `ATN` is `ArcTan`.

The extensions described here are not yet well-documented, and you may need
to understand their implementation to gain an idea of what arguments they need,
or to know whether they work like functions or procedures.
"""

import itertools
import re
import warnings

import mupas_scopes
import mupas_types
import t4050_compiled
import t4050_extensions
import t4050_types

from typing import Annotated, Callable, Optional


# A regular expression for matching against string literals that may contain
# 4050 BASIC I/O addresses.
_RE_IOADDRESS = re.compile(r'"(([@%]\d+)(?:,\d+)?)"')

# A muPas type coding a number in 0..65535
_T_UINT_16 = mupas_types.IntegerSubrange(0, 65535)


def extensions() -> dict[str, mupas_scopes.ExtensionSymbol]:
  """Build and package extensions in this module."""

  # TODO: Investigate certain uses of is_mutable_variable and determine whether
  # they can be relaxed to can_be_lhs.

  result = {
      # Booleans.
      'TRUE': _extension_function('TRUE', '1', [], _boolean),
      'FALSE': _extension_function('FALSE', '0', [], _boolean),

      # A trio of all-time Pascal classics.
      'Ord': _extension_function(  # TODO: Correct this for integer ranges?
          'Ord', '{0}', [mupas_types.Ordinal()], _integer),
      'Pred': _extension_function(
          'Pred', '{0}-1', [mupas_types.Ordinal()], lambda tis: tis[0]),
      'Succ': _extension_function(
          'Succ', '{0}+1', [mupas_types.Ordinal()], lambda tis: tis[0]),

      # Environmental control.
      'AlphaRotate':
          _print_reals_to_address_procedure('AlphaRotate', None, 25, 1),
      'AlphaScale':
          _print_reals_to_address_procedure('AlphaScale', None, 17, 1),
      'Brightness': _extension_procedure(  # 4054 ONLY.
          'Brightness', 'BRI {0}', [mupas_types.IntegerSubrange(0, 3)]),
      'CharSize': _extension_procedure(  # 4054 ONLY.
          'CharSize', 'CHA {0}', [mupas_types.IntegerSubrange(0, 3)]),
      'Font': _extension_procedure(  # Not available on 4051.
          'CharSize', 'FON {0}', [mupas_types.IntegerSubrange(0, 9)]),
      'Font4051': _extension_procedure(  # 4051 ONLY.
          'CharSize', 'PRI @32,18:{0}', [mupas_types.IntegerSubrange(0, 5)]),
      'Fuzz': _try_several_extension_procedures(
          'Fuzz', ['FUZ {0}', 'FUZ {0}{1}'],
          [[mupas_types.Real()], [mupas_types.Real()] * 2]),
      # INIT is not available as it would delete the muPas stack!
      'InternalTapeParams': _extension_procedure(
          'InternalTapeParams', 'PRI @33,0:{0},{1},{2}',
          [mupas_types.Boolean()] * 3),
      'OnPageFull': _extension_procedure(
          'OnPageFull', 'PRI @32,26:{0}', [mupas_types.IntegerSubrange(0, 3)]),
      'UseCrLf': _extension_procedure(
          'UseCrLf', 'PRI @37,26:{0}', [mupas_types.Boolean()]),
      'SetCustomIODelimiters': _extension_procedure(
          'SetCustomIODelimiters', 'PRI @37,0:{0},{1},{2}',
          [mupas_types.IntegerSubrange(0, 255)]),
      'SetDegrees': _extension_procedure('SetDegrees', 'SET DEG', []),
      'SetRadians': _extension_procedure('SetRadians', 'SET RAD', []),
      'SetGrads': _extension_procedure('SetGrads', 'SET GRA', []),
      'SetTrace': _extension_procedure('SetTrace', 'SET TRA', []),
      'SetKey': _extension_procedure('SetKey', 'SET KEY', []),
      'SetNoKey': _extension_procedure('SetNoKey', 'SET NOK', []),
      'SetNoTrace': _extension_procedure('SetNoTrace', 'SET NOR', []),

      # System control operations.
      'Call': _try_several_extension_procedures(
          'Call',
          ['CAL ' + ','.join('{%d}' % a for a in range(i))
           for i in range(1, 22)],
          [[mupas_types.String(6)] + [mupas_types.Type()] * i  # type: ignore
           for i in range(21)]),
      'Copy': _extension_procedure('Copy', 'COP', []),
      'Home': _extension_procedure('Home', 'HOM', []),
      'Page': _extension_procedure('Page', 'PAG', []),

      # 4050 BASIC memory management information.
      'GarbageCollect': _extension_function(  # Garbage collect, then return...
          'GarbageCollect', 'MEM', [], _integer),  # free space available.
      'ProgramTapeSize': _extension_function(
          'ProgramTapeSize', 'SPA', [], _integer),

      # Execution control.
      'Halt': _extension_procedure('Halt', 'STO', []),  # Keeps all prog. state.
      'End': _extension_procedure('End', 'END', []),    # Clears some state.
      'Exit': t4050_extensions.ExtensionExitSymbol(),

      # Interrupts aren't handled by extensions yet.

      # I/O beyond WriteLn and Input has yet to be attempted.
      'Read': _try_several_extension_procedures(
          'Read',
          ['INP {io}' + ','.join('{%d}' % a for a in range(i))
           for i in range(1, 21)],
          [[_Constraints(mupas_types.Type(), is_mutable_variable=True)] * i
           for i in range(1, 21)]),
      'Write': _write_factory(newline=False),
      'WriteLn': _write_factory(newline=True),

      # Scalar math operations.
      'Abs': _extension_function(
          'Abs', 'ABS({0})', [mupas_types.ScalarNumber()], _relax_numeric),
      'ArcCos': _extension_function(
          'ArcCos', 'ACS({0})', [mupas_types.ScalarNumber()], _real),
      'ArcSin': _extension_function(
          'ArcSin', 'ASN({0})', [mupas_types.ScalarNumber()], _real),
      'ArcTan': _extension_function(
          'ArcTan', 'ATN({0})', [mupas_types.ScalarNumber()], _real),
      'Cos': _extension_function(
          'Cos', 'COS({0})', [mupas_types.ScalarNumber()], _real),
      'Exp': _extension_function(
          'Exp', 'EXP({0})', [mupas_types.ScalarNumber()], _real),
      'Int': _extension_function(
          'Int', 'INT({0})', [mupas_types.ScalarNumber()], _integer),
      'Log10': _extension_function(
          'Log10', 'LGT({0})', [mupas_types.ScalarNumber()], _real),
      'Ln': _extension_function(
          'Ln', 'LOG({0})', [mupas_types.ScalarNumber()], _real),
      'Pi': _extension_function('Pi', 'PI', [], _real),
      'Sign': _extension_function(
          'Sign', 'SGN', [mupas_types.ScalarNumber()],
          _integer_subrange(-1, 1)),
      'Sin': _extension_function(
          'Sin', 'SIN({0})', [mupas_types.ScalarNumber()], _real),
      'Sqrt': _extension_function(
          'Sqrt', 'SQRT({0})', [mupas_types.ScalarNumber()], _real),
      'Tan': _extension_function(
          'Tan', 'TAN({0})', [mupas_types.ScalarNumber()], _real),

      # Matrix math operations.
      'MatrixIdentity': _extension_procedure(
          'MatrixIdentity', 'CAL "IDN",{0}',
          [_Constraints(t4050_types.Array2d(None, None, mupas_types.Real()),
                        is_mutable_variable=True)]),
      'MatrixInvert': t4050_extensions.ExtensionProcedureSymbol(_matrix_invert),
      'MatrixMultiply':
          t4050_extensions.ExtensionProcedureSymbol(_matrix_multiply),
      'MatrixTranspose':
          t4050_extensions.ExtensionProcedureSymbol(_matrix_transpose),
      'MatrixSum': _extension_function(
          'MatrixSum', 'SUM({0})',
          [t4050_types.Union(  # Any size array.
              t4050_types.Array1d(None, mupas_types.ScalarNumber()),
              t4050_types.Array2d(None, None, mupas_types.ScalarNumber()))],
              _real),

      # Graphics operations.
      'Axis': _try_several_extension_procedures(
          'Axis',
          ['AXI {io}{0},{1}', 'AXI {io}{0},{1},{2},{3}'],
          [[mupas_types.ScalarNumber()] * 2, [mupas_types.ScalarNumber()] * 4]),
      'Draw': _extension_procedure(
          'Draw', 'DRA {ioprimary}{0},{1}',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber()]),
      'DrawArrays': _array_pair_procedure(
          'DrawArrays', 'DRA {ioprimary}{0},{1}', mupas_types.Real()),
      'DrawGdu': _extension_procedure(
          'DrawGdu', 'PRI @32,20:{0},{1}',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber()]),
      'DrawGduArray': _extension_procedure(
          'DrawGduArray', 'PRI @32,20:{0}',
          [t4050_types.Union(
              # Any length 1-D array with an even number of elements.
              _Constraints(
                  t4050_types.Array1d(None, mupas_types.ScalarNumber()),
                  even_number_of_elements=True),
              # Or, 2-D arrays of any height with two cols.
              t4050_types.Array2d(None, mupas_types.IntegerSubrange(0, 1),
                                  mupas_types.ScalarNumber()),
              t4050_types.Array2d(None, mupas_types.IntegerSubrange(1, 2),
                                  mupas_types.ScalarNumber()),
              t4050_types.Array2d(None, mupas_types.Boolean(),  # lol
                                  mupas_types.ScalarNumber()))]),
      'GraphicInput': _extension_procedure(
          'GraphicInput', 'GIN {ioprimary}{0},{1}',
          [_Constraints(mupas_types.Real(), is_mutable_variable=True),
           _Constraints(mupas_types.Real(), is_mutable_variable=True)]),
      'PageSize':_extension_procedure(
          'PageSize', 'INP {ioprimary}{0},{1}',
          [_Constraints(mupas_types.Real(), is_mutable_variable=True),
           _Constraints(mupas_types.Real(), is_mutable_variable=True)]),
      'MoveCursor': _extension_procedure(
          'MoveCursor', 'MOV {ioprimary}{0},{1}',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber()]),
      'MoveCursorGdu': _print_reals_to_address_procedure(
              'MoveCursorGdu', '@32', 21, 2),
      'Pointer': _extension_procedure(
          'Pointer', 'POI {0},{1},{2}',
          [_Constraints(mupas_types.Real(), is_mutable_variable=True),
           _Constraints(mupas_types.Real(), is_mutable_variable=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, has_room_for=1)]),
      'RelDraw': _extension_procedure(
          'RelDraw', 'RDR {ioprimary}{0},{1}',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber()]),
      'RelMoveCursor': _extension_procedure(
          'RelMoveCursor', 'RMO {ioprimary}{0},{1}',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber()]),
      'Rotate': _extension_procedure(
          'Rotate', 'ROT {0}', [mupas_types.ScalarNumber()]),
      'Scale': _extension_procedure(
          'Scale', 'SCA {0},{1}',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber()]),
      'ViewPort': _extension_procedure(
          'ViewPort', 'VIE {0},{1},{2},{3}',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber(),
           mupas_types.ScalarNumber(), mupas_types.ScalarNumber()]),
      'Window': _extension_procedure(
          'Window', 'WIN {0},{1},{2},{3}',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber(),
           mupas_types.ScalarNumber(), mupas_types.ScalarNumber()]),

      # String operations.
      'Ascii': _try_several_extension_functions(
          'Ascii',
          ['ASC({0})', 'ASC({0},{1})'],  # 2-param form is for A-series only.
          [[t4050_types.String()],
           [t4050_types.String(), mupas_types.Integer()]],
          _integer_subrange(0, 255)),
      'Chr': _extension_function(
          'Chr', 'CHR({0})', [mupas_types.IntegerSubrange(0, 255)], _string(1)),
      'Length': _extension_function(
          'Length', 'LEN({0})', [mupas_types.String(0x10000)], _integer),
      'StringJoin': t4050_extensions.ExtensionProcedureSymbol(_string_join),
      'Pos': _try_several_extension_functions(
          'Pos',
          ['POS({1},{0},1)', 'POS({1},{0},{2})'],
          [[t4050_types.String()] * 2,
           [t4050_types.String()] * 2 + [mupas_types.Integer()]],  # type:ignore
          _integer),
      'Delete': _extension_procedure(  # TODO: basic bounds checking.
          'Delete', 'LET {0}=REP("",{1},{2})',
          [_Constraints(t4050_types.String(), is_mutable_variable=True),
           mupas_types.Integer(), mupas_types.Integer()]),
      'Insert': _extension_procedure(  # TODO: bounds+size checking.
          'Insert', 'LET {1}=REP({0},{2},0)',
          [t4050_types.String(),
           _Constraints(t4050_types.String(), is_mutable_variable=True),
           mupas_types.Integer()]),
      'DeleteInsert': _extension_procedure(  # TODO: bounds+size checking.
          'DeleteInsert', 'LET {0}=REP({3},{1},{2})',
          [_Constraints(t4050_types.String(), is_mutable_variable=True),
           mupas_types.Integer(), mupas_types.Integer(), t4050_types.String()]),
      'CopyTo': _extension_procedure(  # TODO: Size checking for result.
          'CopyTo', 'SEG {0},{1},{2},{3}',
          [t4050_types.String(), mupas_types.Integer(), mupas_types.Integer(),
           _Constraints(t4050_types.String(), is_mutable_variable=True)]),
      'NumToStr': _extension_procedure(
          'NumToStr', '{1}=STR {0}',
          [mupas_types.ScalarNumber(),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, has_room_for=20)]),
      'StrToReal': _extension_function(
          'StrToReal', 'VAL({0})', [t4050_types.String()], _real),

      # 4050 BASIC built-in random number generator.
      'Rand': _extension_function('Rand', 'RND(1)', [], _real),
      'Randomize': _extension_function('Randomize', 'RND(-1)', [], _real),
      'Native4050Rnd': _extension_function(
          'Native4050Rnd', 'RND({0})', [mupas_types.ScalarNumber()], _real),

      # A-model enhancements not defined elsewhere.
      'Dash': _extension_procedure(
          'Dash', 'DAS {0}', [mupas_types.IntegerSubrange(0, 255)]),
      'Alter': _extension_procedure(
          'Alter', 'ALT {0},{1}',
          [t4050_types.String(),
           _Constraints(t4050_types.String(), is_mutable_variable=True)]),
      'Angle': _extension_function(
          'Angle', 'ANG({0},{1})', [mupas_types.Real()] * 2, _real),
      'ArcTan2': _extension_function(  # an alias for POSIX fans...
          'ArcTan2', 'ANG({0},{1})', [mupas_types.Real()] * 2, _real),
      'Area': _array_pair_scalar_function(
          'Area', 'ARE({0},{1})', mupas_types.Real(), _real),
      'BitAnd': _bit_procedure_bitstr_bitstr_bitstr('BitAnd', 'BITAND'),
      'BitOr': _bit_procedure_bitstr_bitstr_bitstr('BitOr', 'BITOR'),
      'BitXor': _bit_procedure_bitstr_bitstr_bitstr('BitXor', 'BITXOR'),
      'BitInvert': t4050_extensions.ExtensionProcedureSymbol(_bitinvert),
      'BitRotate': _bitrotate_or_bitshift(bitrotate=True),
      'BitShift': _bitrotate_or_bitshift(bitshift=True),
      'BitTest': _extension_procedure(
          'BitTest', 'CAL "BITTES",{0},{1},{2}',
          [t4050_types.String(), mupas_types.IntegerSubrange(1, 65535),
           _Constraints(
               t4050_types.Union(mupas_types.Boolean(), mupas_types.Integer()),
               can_be_lhs=True)]),
      'BitSet': _extension_procedure(
          'BitSet', 'CAL "BITSET",{0},{1},{2}',
          [_Constraints(t4050_types.String(), is_mutable_variable=True),
           mupas_types.IntegerSubrange(1, 65535), mupas_types.Boolean()]),
      'PopChar': _extension_procedure(
          'PopChar', 'CCI {0}',
          [_Constraints(t4050_types.String(),
                        is_mutable_variable=True, has_room_for=1)]),
      'Centroid': _array_pair_procedure(
          'Centroid', 'CEN {0},{1},{2},{3}', mupas_types.Real(),
          extras=[_Constraints(mupas_types.Real(), can_be_lhs=True),
                  _Constraints(mupas_types.Real(), can_be_lhs=True)]),
      'Distance': _array_pair_scalar_function(
          'Distance', 'DIS({0},{1})', mupas_types.Real(), _real),
      'Hatch': _array_pair_procedure(
          'Hatch', 'HAT {ioprimary}{0},{1}', mupas_types.Real()),
      'HatchRotate': _extension_procedure(
          'HatchRotate', 'HAT ROT {0}', [mupas_types.Real()]),
      'HatchSpace': _extension_procedure(
          'HatchSpace', 'HAT SPA {0}', [mupas_types.Real()]),
      'HatchAlign': _extension_procedure(
          'HatchAlign', 'HAT ALI {0},{1}', [mupas_types.Real()] * 2),
      'Inside': _array_pair_scalar_function(
          'Inside', 'INS({0},{1},{2},{3})', mupas_types.Real(), _boolean,
          [mupas_types.Real(), mupas_types.Real()]),
      'ModA': _extension_function('ModA', '({0} MOD {1})',
          [mupas_types.ScalarNumber(), mupas_types.ScalarNumber()],
          lambda x: _relax_numeric(x[:1])),  # A-series MOD operator.
      'RowSum': _rowsum_or_columnsum(rowsum=True),
      'ColumnSum': _rowsum_or_columnsum(columnsum=True),
      'Search': _extension_function(
          'Search', 'SEA({0},{1},{2})',
          [t4050_types.String(), t4050_types.String(), mupas_types.Integer()],
          _integer),
      'Translate': _extension_procedure(    # TODO: receiving var is as long
          'Translate', '{2}=TAB({0},{1})',  # as {0}.
          [t4050_types.String(), t4050_types.String(),
           _Constraints(t4050_types.String(), is_mutable_variable=True)]),
      'Strip': _extension_procedure(  # TODO: receiving var is as long as {0}.
          'Strip', '{1}=TRI({0})',
          [t4050_types.String(),
           _Constraints(t4050_types.String(), is_mutable_variable=True)]),
      'NumElements': _extension_function(
          'NumElements', 'UBO({0},-1)',
          [_Constraints(
              t4050_types.Union(
                  t4050_types.Array1d(None, mupas_types.Type()),
                  t4050_types.Array2d(None, None, mupas_types.Type())),
              is_unqualified_variable=True)], _integer),
      'Rows': _extension_function(
          'Rows', 'UBO({0},1)',
          [_Constraints(t4050_types.Array2d(None, None, mupas_types.Type()),
                        is_unqualified_variable=True)], _integer),
      'Columns': _extension_function(
          'Columns', 'UBO({0},2)',
          [_Constraints(t4050_types.Array2d(None, None, mupas_types.Type()),
                        is_unqualified_variable=True)], _integer),
      'MaxLength': _extension_function(
          'MaxLength', 'UBO({0},-1)',
          [_Constraints(t4050_types.String(), is_unqualified_variable=True)],
          _integer),

      # Option 30 refresh graphics.
      'RgBlink': _extension_procedure(
          'RgBlink', 'BLI {0},{1},{2}',
		[_T_UINT_16, mupas_types.Real(), mupas_types.Real()]),
      'RgCursor': _extension_procedure('RgCursor', 'CUR {0}', [_T_UINT_16]),
      'RgFix': _extension_procedure('RgFix', 'FIX {0}', [_T_UINT_16]),
      'RgAppend': _extension_procedure('RgAppend', 'RAP {0}', [_T_UINT_16]),
      'RgClose': _extension_procedure('RgClose', 'RCL', []),
      'RgDelete': _extension_procedure('RgDelete', 'RDE {0}', [_T_UINT_16]),
      'RgInit': _extension_procedure('RgInit', 'RIN', []),
      'RgMemory': _extension_function('RgMemory', 'RME', [], _integer),
      'RgOpen': _extension_procedure('RgOpen', 'ROP {0}', [_T_UINT_16]),
      'RgReplace': _extension_procedure(
          'RgReplace', 'RRE {0},{1}', [_T_UINT_16, _T_UINT_16]),
      'RgSpace': _extension_function('RgSpace', 'RSP', [], _integer),
      'RgSetPoint': _extension_procedure(
          'RgSetPoint', 'STP {0},{1},{2}',
          [_T_UINT_16, mupas_types.Real(), mupas_types.Real()]),
      'RgVisibility': _extension_procedure(
          'RgVisibility', 'VIS {0},{1}', [_T_UINT_16, mupas_types.Boolean()]),

      # R12 graphics enhancement ROM pack.
      'R12Bounds': _extension_procedure(
          'R12Bounds', 'CAL "BOUNDS",{0},{1},{2},{3},{4}',
          [t4050_types.String(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True)]),
      'R12EncodeImageString': _extension_procedure(
          'R12EncodeImageString', 'CAL "CHANGE",{0},{1}',
          [_Constraints(  # TODO: check for sufficient space in the string?
               t4050_types.Union(
                   t4050_types.Array1d(None, mupas_types.Real()),
                   t4050_types.Array2d(None, None, mupas_types.Real())),
               even_number_of_elements=True),
           _Constraints(t4050_types.String(), is_mutable_variable=True)]),
      'R12DecodeImageString': _extension_procedure(
          'R12DecodeImageString', 'CAL "CHANGE",{0},{1}',
          [_Constraints(t4050_types.String(), is_mutable_variable=True),
           _Constraints(  # TODO: check for sufficient space in the array?
               t4050_types.Union(
                   t4050_types.Array1d(None, mupas_types.Real()),
                   t4050_types.Array2d(None, None, mupas_types.Real())),
               even_number_of_elements=True)]),
      'R12DashedGrid': _extension_procedure(
          'R12DashedGrid', 'CAL "DASHED",{0},{1},{2},{3},{4},{5}',
          [mupas_types.Real()] * 6),
      'R12DottedGrid': _extension_procedure(
          'R12DottedGrid', 'CAL "DOTTED",{0},{1},{2},{3},{4},{5}',
          [mupas_types.Real()] * 6),
      'R12AlterPointAsMove': _extension_procedure(
          'R12AlterPointAsMove', 'CAL "DEFINE",{0},-1,{1},{2}',
          [_Constraints(t4050_types.String(), is_mutable_variable=True),
           mupas_types.Real(), mupas_types.Real()]),
      'R12AlterPointAsDraw': _extension_procedure(
          'R12AlterPointAsDraw', 'CAL "DEFINE",{0},1,{1},{2}',
          [_Constraints(t4050_types.String(), is_mutable_variable=True),
           mupas_types.Real(), mupas_types.Real()]),
      'R12LoadImage': _extension_procedure(
          'R12LoadImage', 'CAL "IMAGES",{0}',
          [_Constraints(t4050_types.String(), is_mutable_variable=True)]),
      'R12InjectIntoKbBuffer': _extension_procedure(
          'R12InjectIntoKbBuffer', 'CAL "INPUTS",{0}', [t4050_types.String()]),
      'R12Locate': _extension_procedure(
          'R12Locate', 'CAL "LOCATE",{0},{1},{2},{3}',
          [mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      'R12Music': _extension_procedure(
          'R12Music', 'CAL "MUSIC",{0}', [t4050_types.String()]),
      'R12GetPoint': _extension_procedure(
          'R12GetPoint', 'CAL "POINTS",{0},{1},{2},{3}',
          [t4050_types.String(), mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True)]),
      'R12PrintEscaped': _extension_procedure(
          'R12PrintEscaped', 'CAL "PRINTS",{0}', [t4050_types.String()]),
      'R12Rubber': _extension_procedure(
          'R12Rubber', 'CAL "RUBBER",{0},{1},{2},{3}',
          [mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(), is_mutable_variable=True)]),
      'R12Sounds': _extension_procedure(
          'R12Sounds', 'CAL "SOUNDS",{0}', [t4050_types.String()]),
      'R12EncodeFloats': _extension_procedure(
          'R12EncodeFloats', 'CAL "STRING",{0},{1}',
          [t4050_types.Union(  # TODO: check for sufficient space in the string?
              t4050_types.Array1d(None, mupas_types.ScalarNumber()),
              t4050_types.Array2d(None, None, mupas_types.ScalarNumber())),
           _Constraints(t4050_types.String(), is_mutable_variable=True)]),
      'R12DecodeFloats': _extension_procedure(
          'R12EncodeFloats', 'CAL "STRING",{0},{1}',
          [t4050_types.String(),
           _Constraints(  # TODO: check for sufficient space in the array?
               t4050_types.Union(
                   t4050_types.Array1d(None, mupas_types.ScalarNumber()),
                   t4050_types.Array2d(None, None, mupas_types.ScalarNumber())),
               is_mutable_variable=True)]),
      'R12TogglePointDrawMove': _extension_procedure(
          'R12TogglePointDrawMove', 'CAL "TOGGLE",{0},{1}',
          [_Constraints(t4050_types.String(), can_be_lhs=True),
           _Constraints(mupas_types.Integer(), can_be_lhs=True)]),
      'R12Vertex': _extension_procedure(
          'R12Vertex', 'CAL "VERTEX",{0},{1},{2},{3},{4}',
          [t4050_types.String(),
           mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
  }

  # Add all of the R12 prefixed commands.
  result.update(_r12_prefixed_command_extensions())

  return result


def _r12_prefixed_command_extensions(
) -> dict[str, mupas_scopes.ExtensionSymbol]:
  """Generate the four sets of "prefixed" commands for the R12 ROM pack."""
  result: dict[str, mupas_scopes.ExtensionSymbol] = {}

  # The sets of procedures for three of the prefixes all have the same
  # parameters, so we can generate those proceures with a FOR loop.
  for prefix in ('Abs', 'Gra', 'Rel'):
    result.update({
        f'R12{prefix}Cross': _extension_procedure(
            f'R12{prefix}Cross', f'CAL "{prefix[0]}CROSS",''{0},{1},{2}',
            [mupas_types.Integer(), mupas_types.Real(), mupas_types.Real()]),
        f'R12{prefix}Dots': _extension_procedure(
            f'R12{prefix}Dots', f'CAL "{prefix[0]}DOTS",''{0},{1},{2},{3}',
            [t4050_types.String(), mupas_types.Integer(),
             mupas_types.Real(), mupas_types.Real()]),
        f'R12{prefix}Draw': _extension_procedure(
            f'R12{prefix}Draw', f'CAL "{prefix[0]}DRAW",''{0},{1},{2},{3}',
            [t4050_types.String(), mupas_types.Integer(),
             mupas_types.Real(), mupas_types.Real()]),
        f'R12{prefix}GraphicInput': _extension_procedure(
            f'R12{prefix}GraphicInput',
            f'CAL "{prefix[0]}INPUT",''{0},{1},{2},{3},{4}', 
            [t4050_types.String(), mupas_types.Integer(),
            _Constraints(mupas_types.Real(), can_be_lhs=True),
            _Constraints(mupas_types.Real(), can_be_lhs=True),
             _Constraints(t4050_types.String(),
                          is_mutable_variable=True, might_want_room_for=28)]),
        f'R12{prefix}Move': _extension_procedure(
            f'R12{prefix}Move', f'CAL "{prefix[0]}MOVE",''{0},{1},{2}',
            [_Constraints(t4050_types.String(), is_mutable_variable=True),
             mupas_types.Real(), mupas_types.Real()]),
        f'R12{prefix}Point': _extension_procedure(
            f'R12{prefix}Point', f'CAL "{prefix[0]}POINT",''{0},{1},{2},{3}',
            [t4050_types.String(),
             _Constraints(mupas_types.Integer(), can_be_lhs=True),
             _Constraints(mupas_types.Real(), can_be_lhs=True),
             _Constraints(mupas_types.Real(), can_be_lhs=True)]),
        f'R12{prefix}Print': _extension_procedure(
            f'R12{prefix}Print', f'CAL "{prefix[0]}PRINT",''{0},{1},{2},{3}',
            [t4050_types.String(), mupas_types.Integer(),
             mupas_types.Real(), mupas_types.Real()]),
        f'R12{prefix}Rotate': _extension_procedure(
            f'R12{prefix}Rotate', f'CAL "{prefix[0]}MOVE",''{0},{1},{2},{3}',
            [_Constraints(t4050_types.String(), is_mutable_variable=True),
             mupas_types.Real(), mupas_types.Real(), mupas_types.Real()]),
        f'R12{prefix}Scale': _extension_procedure(
            f'R12{prefix}Scale',
            f'CAL "{prefix[0]}SCALE",''{0},{1},{2},{3},{4}',
            ([_Constraints(t4050_types.String(), is_mutable_variable=True)] +
             [mupas_types.Real()] * 4)),  # type: ignore
        f'R12{prefix}Shear': _extension_procedure(
            f'R12{prefix}Shear',
            f'CAL "{prefix[0]}SHEAR",''{0},{1},{2},{3},{4}',
            ([_Constraints(t4050_types.String(), is_mutable_variable=True)] +
             [mupas_types.Real()] * 4)),  # type: ignore
        f'R12{prefix}Taper': _extension_procedure(
            f'R12{prefix}Taper',
            f'CAL "{prefix[0]}TAPER",''{0},{1},{2},{3},{4}',
            ([_Constraints(t4050_types.String(), is_mutable_variable=True)] +
             [mupas_types.Real()] * 4)),  # type: ignore
    })

  # But the prefixed procedures starting with J use different parameter
  # patterns, mainly in service of retrieving the joystick position.
  result.update({
      'R12JoyCross': _extension_procedure(
          f'R12JoyCross', 'CAL "JCROSS",{0},{1},{2},{3}',
          [mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      'R12JoyDots': _extension_procedure(
          f'R12JoyDots', 'CAL "JDOTS",{0},{1},{2},{3},{4}',
          [t4050_types.String(), mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      'R12JoyDots': _extension_procedure(
          f'R12JoyDots', 'CAL "JDOTS",{0},{1},{2},{3},{4}',
          [t4050_types.String(), mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      'R12JoyGraphicInput': _extension_procedure(
          'R12JoyGraphicInput', 'CAL "JINPUT",{0},{1},{2},{3},{4}',
          [t4050_types.String(), mupas_types.Integer(),
           mupas_types.Real(), mupas_types.Real(),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      f'R12JoyPoint': _extension_procedure(
          f'R12JoyPoint', 'CAL "JPOINT",{0},{1},{2},{3},{4},{5}',
          [t4050_types.String(), mupas_types.Integer(),
           _Constraints(mupas_types.Integer(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      f'R12JoyPrint': _extension_procedure(
          f'R12JoyPrint', 'CAL "JPRINT",{0},{1},{2},{3},{4}',
          [t4050_types.String(), mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      f'R12JoyMove': _extension_procedure(
          f'R12JoyMove', 'CAL "JMOVE",{0},{1},{2},{3},{4}',
          [t4050_types.String(), mupas_types.Integer(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      f'R12JoyRotate': _extension_procedure(
          f'R12JoyRotate', 'CAL "JROTATE",{0},{1},{2},{3},{4},{5}',
          [t4050_types.String(), mupas_types.Integer(), mupas_types.Real(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      f'R12JoyScale': _extension_procedure(
          f'R12JoyScale', 'CAL "JSCALE",{0},{1},{2},{3},{4},{5},{6}',
          [t4050_types.String(), mupas_types.Integer(),
           mupas_types.Real(), mupas_types.Real(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      f'R12JoyShear': _extension_procedure(
          f'R12JoyShear', 'CAL "JSHEAR",{0},{1},{2},{3},{4},{5},{6}',
          [t4050_types.String(), mupas_types.Integer(),
           mupas_types.Real(), mupas_types.Real(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
      f'R12JoyTaper': _extension_procedure(
          f'R12JoyTaper', 'CAL "JTAPER",{0},{1},{2},{3},{4},{5},{6}',
          [t4050_types.String(), mupas_types.Integer(),
           mupas_types.Real(), mupas_types.Real(),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(mupas_types.Real(), can_be_lhs=True),
           _Constraints(t4050_types.String(),
                        is_mutable_variable=True, might_want_room_for=28)]),
  })

  return result


###################################
### Extension-building helpers. ###
###################################


class _Constraints(mupas_types.Type):
  """Additional type-checking constraints for procedure parameters.

  Function parameters seem not to need this kind of checking.

  These constraints are not carefully organised --- they've been introduced
  as needs have emerged. Some apply only to specific types.
  """
  wrapped: mupas_types.Type
  can_be_lhs: bool
  is_unqualified_variable: bool
  has_room_for: Optional[int] = None
  might_want_room_for: Optional[int] = None
  even_number_of_elements: bool
  access_matches: Optional[str] = None

  def __init__(
      self,
      wrapped: mupas_types.Type,
      *,
      can_be_lhs: bool = False,
      is_unqualified_variable: bool = False,
      is_mutable_variable: bool = False,
      has_room_for: Optional[int] = None,
      might_want_room_for: Optional[int] = None,
      even_number_of_elements: bool = False,
      access_matches: Optional[str] = None
  ):
    """Initialise a _Constraints.

    Args:
      wrapped: Type for which we express additional constraints.
      can_be_lhs: Same as t4050_compiled.Expression.can_be_lhs.
      is_unqualified_variable: Same as in t4050_compiled.Expression.
      is_mutable_variable: Shorthand for can_be_lhs and is_unqualified_variable.
      has_room_for: (String, Array1d) Does the string or array have at least
          this many elements? (The name anticipates using this for variables.)
          If it doesn't, it's a fatal error.
      might_want_room_for: (String, Array1d) Like `has_room_for`, but only
          prints an error if there's a violation.
      even_number_of_elements: (String, Array1d) Does the string or array have
          an even number of elements?
      access_matches: The Expression's access attribute fullmatches this
          regular expression. (Kludgy!)
    """
    self.can_be_lhs = can_be_lhs or is_mutable_variable
    self.is_unqualified_variable = (
        is_unqualified_variable or is_mutable_variable)
    self.has_room_for = has_room_for
    self.might_want_room_for = might_want_room_for
    self.even_number_of_elements = even_number_of_elements
    self.access_matches = access_matches

  def __str__(self):
    text = []
    for k, v in self.__dict__.items():
      if isinstance(v, bool):
        if v: text.append(k)
      else:
        if v is not None: text.append(f'{k}={v}')
    return ','.join(text)


# A type for a function that computes a return type from argument types.
_TypeInfoComputer = Callable[[list[mupas_types.Type]], mupas_types.Type]


def _extension_procedure(
    name: str,
    template: str | list[str],
    arg_typeinfo: list[mupas_types.Type],
    indent: bool = True,
) -> t4050_extensions.ExtensionProcedureSymbol:
  """A factory function for building various simple procedure-like extensions.

  Extensions made by this factory return t4050_compiled.Statement objects.

  Template strings (see the `template` argument below) may contain the keyword
  format placeholders `{io}` and `{ioprimary}`. If either is present in any
  template string, the extension may optionally take an extra initial parameter
  in the form of a 4050 BASIC I/O address ('@12', '%12,34' and the like).
  For the above, `{io}` will contain '@12:' and '%12,34:' respectively, while
  for `{ioprimary}`, it's only '@12:' and '%12:'. Note the trailing colon.

  If no such initial parameter appears to be present, then `{io}` and
  `{ioprimary}` will both be empty strings, and all the parameters will be
  processed in accordance with `arg_typeinfo`.

  If only `{ioprimary}` is present in the template strings, then an initial
  I/O address parameter literal will raise an exception if it contains a
  secondary address.

  Args:
    name: Name of this extension, for error messages.
    template: A format string template for a single new line or multiple new
        lines of code generated by this extension. Arguments to the extension
        are substituted into the template string(s) as positional format
        placeholders: `{0}`, `{1}`, `{2}`, etc. See also note above about
        `{io}` and `{ioprimary}` placeholders.
    arg_typeinfo: A list of expected types for arguments to the extension.
        Types supplied in the prgram are checked against these types using the
        muPas's internal type compatibility checker for scalar and element-wise
        matrix operations.
    indent: If True (the default), the template string(s) will be automatically
        indented by six spaces.

  Returns:
    A 4050 BASIC muPas extension that behaves like a procedure.
  """
  # Convert the template into a list if it's a bare string.
  if isinstance(template, str): template = [template]
  # Perform indentation of template strings if requested.
  if indent: template = [f'      {t}' for t in template]

  # See if the extension wishes for optional I/O address argument capability.
  want_io, want_ioprimary = False, False
  # Detect whether any template string contains {io} or {ioprimary} by doing
  # trial substitutions with a gibberish string. If the gibberish string is
  # present in a template to start out with, abandon such substitution.
  odd = r',$&"!@%!([)]beanS$'  # An unlikely string!
  no_oddness = not any(odd in t for t in template)
  want_io = no_oddness and any(
     odd in t.format(*range(50), io=odd, ioprimary='') for t in template)
  want_ioprimary = no_oddness and any(
     odd in t.format(*range(50), io='', ioprimary=odd) for t in template)

  # Define the function that implements the extension.
  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Statement:
    # If the extension wants I/O address capability, scrutinise any "extra"
    # first argument to see if it looks like an I/O address. If it does, save
    # the I/O address information in a form suitable for BASIC.
    io, ioprimary = '', ''
    if len(args) == len(arg_typeinfo) + 1 and args[0].is_quoted_string_literal:
      io, ioprimary = _collect_io_address(
          name, args[0], want_io, want_ioprimary)
    # We got an I/O address in the first argument, so delete it from the
    # argument list in preparation for conventional argument processing.
    if io: args = args[1:]  # No pop() to avoid mutating args.

    # Now back to conventional argument processing.
    if len(arg_typeinfo) != len(args): raise Error(
        f'{name} requires {len(arg_typeinfo)} argument(s), not {len(args)}')
    for expected, actual in zip(arg_typeinfo, args):
      _check_expression_compatibility(expected, actual)
    statements = [t4050_compiled.Statement(
        code=arg.compute, stack_growth=arg.stack_growth) for arg in args]
    statements.append(t4050_compiled.Statement(
        code=[t.format(*(arg.access for arg in args),
                       io=io, ioprimary=ioprimary) for t in template]))
    return t4050_compiled.chain_statements(statements)

  # Return the new extension.
  return t4050_extensions.ExtensionProcedureSymbol(extension=extension)


def _try_several_extension_procedures(
    name: str,
    templates: list[str | list[str]],
    arg_typeinfos: list[list[mupas_types.Type]],
    indents: bool | list[bool] = True,
) -> t4050_extensions.ExtensionProcedureSymbol:
  """Try several different _extension_procedure configurations.

  Some extensions should take multiple configurations of parameters but still
  use the same name (e.g. Axis, which can take either two parameters or four).
  Others may take different types. This helper allows you to try extensions
  made by several sets of `_extension_procedure` arguments serially until one
  of them succeeds, which may be useful for some extensions. The sets are
  attempted in the order provided, which may be important to you.

  Args:
    name: Name of this extension, for error messages.
    templates: N candidate `_extension_procedure` `template` arguments.
    arg_typeinfos: N candidate `arg_typeinfo` arguments.
    indents: N candidate `indent` arguments. Can be abbreviated to a single
        value if all N would be the same.

  Returns:
    A 4050 BASIC muPas extension that behaves like a procedure.
  """
  # Turn indents into a list if there's just one value, make sure all the list
  # arguments are the same length, and bundle the arguments together in
  # preparation for trying them all.
  num_extensions = len(templates)
  if isinstance(indents, bool): indents = [indents] * num_extensions
  assert num_extensions == len(arg_typeinfos) == len(indents)
  bundled_args = zip(itertools.repeat(name), templates, arg_typeinfos, indents)

  # Build each of the extensions.
  extensions = [_extension_procedure(*args) for args in bundled_args]

  # Return an extension that tries all of the extensions in turn. If no success
  # with any of them, report all the errors encountered with each extension.
  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Statement:
    errors: list[str] = []
    for ext in extensions:
      try:
        return ext.extension(args)
      except (Error, t4050_types.TypeMismatch) as e:
        errors.append(str(e))
    else:
      errors_gallery = '\n   ' + '\n   '.join(errors)
      raise Error(
          f'Failed to apply any of the {num_extensions} extensions under the '
          f'name {name}. Errors encountered were:{errors_gallery}')

  return t4050_extensions.ExtensionProcedureSymbol(extension=extension)


def _extension_function(
    name: str,
    template: str,
    arg_typeinfo: list[mupas_types.Type],
    compute_result_typeinfo: _TypeInfoComputer,
) -> t4050_extensions.ExtensionFunctionSymbol:
  """A factory function for building various simple function-like extensions.

  Extensions made by this factory return t4050_compiled.Expression objects.
  These objects introduce no new code in the `.compute` member but instead
  specify a new value for the `.access` member. The result type of the
  expression is computed by a function supplied by the caller.

  Args:
    name: Name of this extension, for error messages.
    template: A format string template for the returned expression's `.access`
        member. Arguments to the extension are substituted into the template
        string as positional format placeholders: `{0}`, `{1}`, `{2}`, etc.
    arg_typeinfo: A list of expected types for arguments to the extension.
        Types supplied in the prgram are checked against these types using the
        muPas's internal type compatibility checker for scalar and element-wise
        matrix operations.
    compute_result_typeinfo: A function that computes the type of the
        expression returned by the extension given its arguments. This module
        includes some frequently-used type computing functions.

  Returns:
    A 4050 BASIC muPas extension that behaves like a function.
  """
  # Define the function that implements the extension.
  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Expression:
      if len(arg_typeinfo) != len(args): raise Error(
          f'{name} requires {len(arg_typeinfo)} argument(s), not {len(args)}')
      for expected, actual in zip(arg_typeinfo, args):
        _check_expression_compatibility(expected, actual)
      return t4050_compiled.Expression(
          typeinfo=compute_result_typeinfo([a.typeinfo for a in args]),
          access=template.format(*(arg.access for arg in args)))

  # Return the new extension.
  return t4050_extensions.ExtensionFunctionSymbol(extension=extension)


def _try_several_extension_functions(
    name: str,
    templates: list[str],
    arg_typeinfos: list[list[mupas_types.Type]],
    compute_result_typeinfos: _TypeInfoComputer | list[_TypeInfoComputer],
) -> t4050_extensions.ExtensionFunctionSymbol:
  """As `_try_several_extension_procedures`, but for functions.

  Args:
    name: Name of this extension, for error messages.
    templates: N candidate `_extension_function` `template` arguments.
    arg_typeinfos: N candidate `arg_typeinfo` arguments.
    compute_result_typeinfos: N candidate `compute_result_typeinfo` arguments.
        Can be abbreviated to a single value if all N would be the same.

  Returns:
    A 4050 BASIC muPas extension that behaves like a function.
  """
  # Turn compute_result_typeinfos into a list if there's just one value, make
  # sure all the list arguments are the same length, and bundle the arguments
  # together in preparation for trying them all.
  num_extensions = len(templates)
  if not isinstance(compute_result_typeinfos, list):
    compute_result_typeinfos = [compute_result_typeinfos] * num_extensions
  assert num_extensions == len(arg_typeinfos) == len(compute_result_typeinfos)
  bundled_args = zip(itertools.repeat(name), templates,
                     arg_typeinfos, compute_result_typeinfos)

  # Build each of the extensions.
  extensions = [_extension_function(*args) for args in bundled_args]

  # Return an extension that tries all of the extensions in turn. If no success
  # with any of them, report all the errors encountered with each extension.
  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Expression:
    errors: list[str] = []
    for ext in extensions:
      try:
        return ext.extension(args)
      except (Error, t4050_types.TypeMismatch) as e:
        errors.append(str(e))
    else:
      errors_gallery = '\n   ' + '\n   '.join(errors)
      raise Error(
          f'Failed to apply any of the {num_extensions} extensions under the '
          f'name {name}. Errors encountered were:{errors_gallery}')

  return t4050_extensions.ExtensionFunctionSymbol(extension=extension)


def _real(arg_typeinfo: list[mupas_types.Type]) -> mupas_types.Real:
  """Ignores argument types; designates return types as Real."""
  del arg_typeinfo  # Unused.
  return mupas_types.Real()


def _boolean(arg_typeinfo: list[mupas_types.Type]) -> mupas_types.Boolean:
  """Ignores argument types; designates return types as Integer."""
  del arg_typeinfo  # Unused.
  return mupas_types.Boolean()


def _integer(arg_typeinfo: list[mupas_types.Type]) -> mupas_types.Integer:
  """Ignores argument types; designates return types as Integer."""
  del arg_typeinfo  # Unused.
  return mupas_types.Integer()


def _integer_subrange(lower_bound: int, upper_bound: int) -> _TypeInfoComputer:
  """Ignores argument types; designates return types as IntegerSubrange."""

  def type_info_computer(
      arg_typeinfo: list[mupas_types.Type]
  ) -> mupas_types.IntegerSubrange:
    del arg_typeinfo  # Unused.
    return mupas_types.IntegerSubrange(lower_bound, upper_bound)

  return type_info_computer


def _relax_numeric(
    arg_typeinfo: Annotated[list[mupas_types.Type], 1],
) -> mupas_types.ScalarNumber:
  """Relax a scalar numeric type to a (conservatively) similar type.

  For general use on functions on scalar numbers which can't offer too many
  guarantees about the nature of their results. Ordinal types will remain
  ordinals and reals will remain reals, but in general all guarantees about
  upper and lower value limits are abandoned. So, for example, a Char (or even
  a Boolean) will relax to an Integer.

  Args:
    arg_typeinfo: A length-1 list of types. To avoid an exception, make certain
        that these types are subtypes of `mupas_types.ScalarNumber`.

  Returns:
    The relaxed type, as described.
  """
  match arg_typeinfo[0]:
    case mupas_types.LongintSubrange():
      return mupas_types.Longint()
    case mupas_types.IntegerSubrange():
      return mupas_types.Integer()
    case mupas_types.Longint():
      return mupas_types.Longint()
    case mupas_types.Integer() | mupas_types.Char() | mupas_types.Boolean():
      return mupas_types.Integer()
    case mupas_types.Real():
      return mupas_types.Real()
    case _:
      raise Error(
          f'Internal error in _relax_numeric: {arg_typeinfo[0]}')


def _string(length: int) -> _TypeInfoComputer:
  """Ignores argument types; designates return types as String."""

  def type_info_computer(
      arg_typeinfo: list[mupas_types.Type]
  ) -> mupas_types.String:
    del arg_typeinfo  # Unused.
    return mupas_types.String(length)

  return type_info_computer


##########################################
### Special extension implementations. ###
##########################################


def _write_factory(newline: bool) -> t4050_extensions.ExtensionProcedureSymbol:
  """Extension factory for Write and WriteLn."""
  name = 'WriteLn' if newline else 'Write'
  final_semicolon = '' if newline else ';'
  return _try_several_extension_procedures(
    name,
    ['PRI {io}' + ';'.join('{%d}' % a for a in range(i)) + final_semicolon
     for i in range(21)],
    [[t4050_types.Union(t4050_types.String(), mupas_types.ScalarNumber())] * i
     for i in range(21)])


def _print_reals_to_address_procedure(
    name: str,
    default_primary_address: Optional[str],
    secondary_address: int,
    num_reals,
) -> t4050_extensions.ExtensionProcedureSymbol:
  """Extension factory for a common pattern.

  The pattern is a PRINT statement to a user-chosen primary address (or a
  default if specified) and a fixed secondary address, followed by some number
  of real values.

  Args:
    name: Name of the extension, for error messages.
    default_primary_address: The default primary address if left unspecified
        by the user. Set to None to force the user to specify something.
    secondary_address: Fixed secondary address.
    num_reals: Number of reals required by the extension.

  Returns:
    A function implementing an ExtensionProcedureSymbol.
  """
  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Statement:
    # Check argument length.
    valid_num_args = ((num_reals + 1,) if default_primary_address is None else
                      (num_reals + 1, num_reals))
    if len(args) not in valid_num_args: raise Error(
        f'{name} can have {" or ".join(valid_num_args)} parameters, not '
        f'{len(args)}')

    # Check and gather information for address.
    def address_fail():
      raise Error(
          f'{"Optional f" if default_primary_address is None else "F"}irst '
          f'argument to {name} must be a primary I/O address less the colon, '
          f"e.g. '@12', '%40'.")

    if len(args) == num_reals + 1:  # User did supply an address,
      arg, args = args[0], args[1:]  # No pop() to avoid mutating args.
      if not isinstance(arg.typeinfo, mupas_types.String): address_fail()
      m = re.fullmatch(f'"([@%]\d+)"', arg.access)
      if not m:
        address_fail()
      else:
        primary_address = m.group(1)

    else:  # User did not supply an address.
      assert isinstance(default_primary_address, str)  # mypy...
      primary_address = default_primary_address
    address = f'{primary_address},{secondary_address}'

    # Check data parameters.
    for arg in args:
      t4050_types.check_assignment_or_parameter_compatibility(
          mupas_types.Real(), arg.typeinfo)

    # Assemble and return statement.
    code = (list(itertools.chain(*(arg.compute for arg in args))) +
            [f'      PRI {address}:' + ','.join(arg.access for arg in args)])
    stack_growth = sum(arg.stack_growth for arg in args)
    return t4050_compiled.Statement(code, stack_growth)

  return t4050_extensions.ExtensionProcedureSymbol(extension=extension)


def _array_pair_procedure(
    name: str,
    template: str | list[str],
    element_typeinfo: mupas_types.Type | list[mupas_types.Type],
    indent: bool = True,
    extras: Optional[list[mupas_types.Type]] = None,
) -> t4050_extensions.ExtensionProcedureSymbol:
  """Create an extension procedure with two same-size array arguments.

  For procedures like array-argument Draw and (for 4052A/4054A) Hatch, which
  take two array arguments that must be the same size. Operates much like
  _extension_procedure, with similar arguments. The returned extension will
  allow its two array arguments to be differing dimensions (1-D or 2-D, doesn't
  matter which), but it will raise an error if the arguments have differing
  numbers of elements.

  Args:
    name: Name of this extension, for error messages.
    template: Similar to the `template` argument to `_extension_procedure`;
        note that only the `{0}`, `{1}`, `{io}`, and `{ioprimary}` placeholders
        are valid for all calls; additional placeholders starting from `{2}`
        are available if you supply a nonempty `extras`.
    element_typeinfo: The array pair should have elements of this type, or if
        a list, array 0 should have elements of type `element_typeinfo[0]`
        and analogously for array 1.
    indent: As for `_extension_procedure`.
    extras: An optional list of types for optional additional arguments to the
        extension procedure following the two array arguments.

  Returns:
    A 4050 BASIC muPas extension that behaves like a procedure.
  """
  # Expand element typeinfo to a pair if needed.
  element_typeinfos = (element_typeinfo if isinstance(element_typeinfo, list)
                       else [element_typeinfo, element_typeinfo])

  # Arguments to the extension we make must be array variables. Either 1-D or
  # 2-D is fine.
  typeinfo = _Constraints(
      t4050_types.Union(t4050_types.Array1d(None, element_typeinfos[0]),
                        t4050_types.Array2d(None, None, element_typeinfos[1])),
      is_unqualified_variable=True)

  # Here's an extension implementation that doesn't check for size agreement of
  # the two argument arrays.
  typeinfos: list[mupas_types.Type] = [typeinfo, typeinfo]
  if extras is not None: typeinfos.extend(extras)
  bare_extension = _extension_procedure(name, template, typeinfos, indent)

  # Here's the extension we'll return, which wraps the one we just made in
  # more checks, particularly for the same number of elements.
  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Statement:
    # Try the bare extension first, which does a lot of arg checking.
    statement = bare_extension.extension(args)
    # Perform a size agreement check on the parameters and return if it passes.
    x, y = args[-2].typeinfo, args[-1].typeinfo
    assert isinstance(x, (mupas_types.Array1d, mupas_types.Array2d))  # For
    assert isinstance(y, (mupas_types.Array1d, mupas_types.Array2d))  # mypy...
    if array_num_elements(x) != array_num_elements(y): raise Error(
        f'{name} requires both array arguments to have the same number '
        'of elements.')
    return statement

  # Return the new extension.
  return t4050_extensions.ExtensionProcedureSymbol(extension=extension)


def _array_pair_scalar_function(
    name: str,
    template: str,
    element_typeinfo: mupas_types.Type | list[mupas_types.Type],
    compute_result_typeinfo: _TypeInfoComputer,
    extras: Optional[list[mupas_types.Type]] = None,
) -> t4050_extensions.ExtensionFunctionSymbol:
  """Create an extension scalar function of two same-size array argments.

  For functions like (for 4052A/4054A) Area, which take two array arguments
  that must be the same size. Operates much like _extension_function, with
  similar arguments. The returned extension will allow its two array arguments
  to be differing dimensions (1-D or 2-D, doesn't matter which), but it will
  raise an error if the arguments have differing numbers of elements.

  Args:
    name: Name of this extension, for error messages.
    template: Similar to the `template` argument to `_extension_procedure`;
        note that only the `{0}`, `{1}`, `{io}`, and `{ioprimary}` placeholders
        are valid for all calls; additional placeholders starting from `{2}`
        are available if you supply a nonempty `extras`.
    element_typeinfo: The array pair should have elements of this type, or if
        a list, array 0 should have elements of type `element_typeinfo[0]`
        and analogously for array 1.
    compute_result_typeinfo: As for `_extension_function`.
    extras: An optional list of types for optional additional arguments to the
        extension functionfollowing the two array arguments.

  Returns:
    A 4050 BASIC muPas extension that behaves like a function.
  """
  # Expand element typeinfo to a pair if needed.
  element_typeinfos = (element_typeinfo if isinstance(element_typeinfo, list)
                       else [element_typeinfo, element_typeinfo])

  # Arguments to the extension we make must be array variables. Either 1-D or
  # 2-D is fine.
  typeinfo = _Constraints(
      t4050_types.Union(t4050_types.Array1d(None, element_typeinfos[0]),
                        t4050_types.Array2d(None, None, element_typeinfos[1])),
      is_unqualified_variable=True)

  # Here's an extension implementation that doesn't check for size agreement of
  # the two argument arrays.
  typeinfos: list[mupas_types.Type] = [typeinfo, typeinfo]
  if extras is not None: typeinfos.extend(extras)
  bare_extension = _extension_function(
      name, template, typeinfos, compute_result_typeinfo)

  # Here's the extension we'll return, which wraps the one we just made in
  # more checks, particularly for the same number of elements.
  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Expression:
    # Try the bare extension first, which does a lot of arg checking.
    expression = bare_extension.extension(args)
    # Perform a size agreement check on the parameters and return if it passes.
    x, y = args[-2].typeinfo, args[-1].typeinfo
    assert isinstance(x, (mupas_types.Array1d, mupas_types.Array2d))  # For
    assert isinstance(y, (mupas_types.Array1d, mupas_types.Array2d))  # mypy...
    if array_num_elements(x) != array_num_elements(y): raise Error(
        f'{name} requires both array arguments to have the same number '
        'of elements.')
    return expression

  # Return the new extension.
  return t4050_extensions.ExtensionFunctionSymbol(extension=extension)


def _matrix_invert(
    args: list[t4050_compiled.Expression],
) -> t4050_compiled.Statement:
  """Extension for MatrixInvert."""
  if len(args) not in (2, 3): raise Error(
      'MatrixInvert takes two matrix variable parameters and an optional '
      'numeric variable parameter for the determinant')
  code: list[str] = []
  stack_growth = 0

  # Check matrix arguments and generate inversion statement.
  # Are parameters 1 and 2 array variables of reals?
  _check_expression_compatibility(
      _Constraints(
          t4050_types.Array2d(None, None, mupas_types.Real()),
          is_unqualified_variable=True), args[0])
  _check_expression_compatibility(
      _Constraints(
          t4050_types.Array2d(None, None, mupas_types.Real()),
          is_mutable_variable=True), args[1])
  typeinfo_in = args[0].typeinfo
  typeinfo_out = args[1].typeinfo
  assert isinstance(typeinfo_in, mupas_types.Array2d)  # mypy...
  assert isinstance(typeinfo_out, mupas_types.Array2d)
  # Are they both the same size?
  t4050_types.check_assignment_or_parameter_compatibility(
      typeinfo_in, typeinfo_out)
  # Is the input matrix at least as wide as it is tall?
  rows, cols = array2d_dims(typeinfo_in)
  if rows > cols: raise Error(
     'Array arguments to MatrixInvert must be at least as wide as they are '
     f'tall; this array is {rows}x{cols}, which is too tall')
  code.extend(args[0].compute)
  code.extend(args[1].compute)
  code.append(f'      {args[1].access}=INV {args[0].access}')
  stack_growth = args[0].stack_growth + args[1].stack_growth

  # Check optional determinant argument and generate determinant statement.
  if len(args) == 3:
    _check_expression_compatibility(
        _Constraints(mupas_types.Real(), can_be_lhs=True), args[2])
    code.extend(args[2].compute)
    code.append(f'      {args[2].access}=DET')
    stack_growth += args[2].stack_growth

  return t4050_compiled.Statement(code=code, stack_growth=stack_growth)


def _matrix_multiply(
    args: list[t4050_compiled.Expression],
) -> t4050_compiled.Statement:
  """Extension for MatrixMultiply."""
  if len(args) != 3: raise Error(
      'MatrixMultiply takes three matrix variable arguments')

  # Check matrix arguments.
  # Are parameters 1 and 2 array variables of reals? Is the third parameter a
  # mutable array variable of reals?
  _check_expression_compatibility(
      _Constraints(
          t4050_types.Array2d(None, None, mupas_types.Real()),
          is_unqualified_variable=True), args[0])
  _check_expression_compatibility(
      _Constraints(
          t4050_types.Array2d(None, None, mupas_types.Real()),
          is_unqualified_variable=True), args[1])
  _check_expression_compatibility(
      _Constraints(
          t4050_types.Array2d(None, None, mupas_types.Real()),
          is_mutable_variable=True), args[2])
  typeinfo_a = args[0].typeinfo  # C = A * B
  typeinfo_b = args[1].typeinfo
  typeinfo_c = args[2].typeinfo
  assert isinstance(typeinfo_a, mupas_types.Array2d)  # mypy...
  assert isinstance(typeinfo_b, mupas_types.Array2d)
  assert isinstance(typeinfo_c, mupas_types.Array2d)
  # Are the sizes correct?
  rows_a, cols_a = array2d_dims(typeinfo_a)
  rows_b, cols_b = array2d_dims(typeinfo_b)
  rows_c, cols_c = array2d_dims(typeinfo_c)
  if cols_a != rows_b or rows_a != rows_c or cols_b != cols_c:
    raise Error('Incompatible array sizes for MatrixMultiply')

  # Generate code for matrix multiplication.
  code = args[0].compute + args[1].compute + args[2].compute + [
      f'      {args[2].access}={args[0].access} MPY {args[1].access}']
  stack_growth = (
      args[0].stack_growth + args[1].stack_growth + args[2].stack_growth)

  return t4050_compiled.Statement(code=code, stack_growth=stack_growth)


def _matrix_transpose(
    args: list[t4050_compiled.Expression],
) -> t4050_compiled.Statement:
  """Extension for MatrixTranspose."""
  if len(args) != 2: raise Error(
      'MatrixTranspose takes two matrix variable arguments')

  # Check matrix arguments.
  # Are both parameters array variables? Is the second one mutable?
  _check_expression_compatibility(
      _Constraints(
          t4050_types.Array2d(None, None, mupas_types.ScalarNumber()),
          is_unqualified_variable=True), args[0])
  _check_expression_compatibility(
      _Constraints(
          t4050_types.Array2d(None, None, mupas_types.ScalarNumber()),
          is_mutable_variable=True), args[1])
  typeinfo_r = args[0].typeinfo
  typeinfo_l = args[1].typeinfo
  assert isinstance(typeinfo_r, mupas_types.Array2d)  # mypy...
  assert isinstance(typeinfo_l, mupas_types.Array2d)
  # Are the sizes correct?
  rows_r, cols_r = array2d_dims(typeinfo_r)
  rows_l, cols_l = array2d_dims(typeinfo_l)
  if rows_l != cols_r or cols_l != rows_r: raise Error(
      'Incompatible array sizes for MatrixTranspose')

  # Generate code for matrix transpose.
  code = args[0].compute + args[1].compute + [
      f'      {args[1].access}=TRN {args[0].access}']
  stack_growth = args[0].stack_growth + args[1].stack_growth

  return t4050_compiled.Statement(code=code, stack_growth=stack_growth)


def _string_join(
    args: list[t4050_compiled.Expression],
) -> t4050_compiled.Statement:
  """Extension for StringJoin."""
  if len(args) != 3: raise Error(
      'StringJoin takes two string expression arguments and '
      'a string variable destination')
  # Are parameters 0 and 1 strings?
  _check_expression_compatibility(t4050_types.String(), args[0])
  _check_expression_compatibility(t4050_types.String(), args[1])
  # Is parameter 2 big enough to hold the longest possible string that could
  # be made by concatenating parameters 0 and 1? If not, print a warning.
  # Is it a variable we can mutate?
  assert isinstance(args[0].typeinfo, mupas_types.String)  # mypy...
  assert isinstance(args[1].typeinfo, mupas_types.String)
  max_room_needed = args[0].typeinfo.length + args[1].typeinfo.length
  _check_expression_compatibility(
      _Constraints(t4050_types.String(),
                   is_mutable_variable=True,
                   might_want_room_for=max_room_needed), args[2])
  return t4050_compiled.Statement(
      code=args[0].compute + args[1].compute + [
          f'{args[2].access}={args[0].access}&{args[1].access}'],
      stack_growth=args[0].stack_growth)


def _bit_procedure_bitstr_bitstr_bitstr(
    name: str, what_to_call: str,
) -> t4050_extensions.ExtensionProcedureSymbol:
  """Extension factory for the -A models' BITAND, BITOR, BITXOR calls."""
  # Start with an extension implementation that lacks useful checks.
  bare_extension = _extension_procedure(
      name, f'CAL "{what_to_call}"'',{0},{1},{2}',
      [t4050_types.String(), t4050_types.String(),
       _Constraints(t4050_types.String(), is_mutable_variable=True)])

  # We'll return this extension, which wraps the one we just made in advisory
  # checks, particularly for making certain that the destination variable has
  # enough room.
  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Statement:
    # Try the bare extension first, which does a lot of arg checking.
    statement = bare_extension.extension(args)
    # Check whether the destination argument will always have as much room as
    # it might need to store the result.
    assert isinstance(args[0].typeinfo, mupas_types.String)  # for mypy...
    assert isinstance(args[1].typeinfo, mupas_types.String)
    assert isinstance(args[2].typeinfo, mupas_types.String)
    param_max_length = max(args[0].typeinfo.length, args[1].typeinfo.length)
    destination_length = args[2].typeinfo.length
    if destination_length < param_max_length: warnings.warn(
        f'In call to {name}: the destination string with maximum length '
        f'{destination_length} might be asked to store a result string of '
        f'size {param_max_length}! Odd behaviour or program termination could '
        'result')
    return statement

  # Return the new extension.
  return t4050_extensions.ExtensionProcedureSymbol(extension=extension)


def _bitinvert(
    args: list[t4050_compiled.Expression],
) -> t4050_compiled.Statement:
  """Extenson for BitInvert."""
  if len(args) != 2: raise Error(
      'BitInvert parameters are an input string and an output string variable')

  # Check input arguments and generate bit complement statement.
  _check_expression_compatibility(t4050_types.String(), args[0])
  _check_expression_compatibility(
      _Constraints(t4050_types.String(), is_mutable_variable=True), args[1])
  typeinfo_in = args[0].typeinfo
  typeinfo_out = args[1].typeinfo
  assert isinstance(typeinfo_in, mupas_types.String)  # mypy...
  assert isinstance(typeinfo_out, mupas_types.String)
  if typeinfo_out.length < typeinfo_in.length: warnings.warn(
      'In call to BitInvert: the destination string with maximum length '
      f'{typeinfo_out.length} might be asked to store a result string of size '
      f'{typeinfo_in.length}! Odd behaviour or program termination could '
      'result')

  # Create statement result.
  code = args[0].compute + args[1].compute + [
      f'      CAL "BITCMP",{args[0].access},{args[1].access}']
  stack_growth = args[0].stack_growth + args[1].stack_growth
  return t4050_compiled.Statement(code=code, stack_growth=stack_growth)


def _bitrotate_or_bitshift(
    bitrotate: bool = False, bitshift: bool = False,
) -> t4050_extensions.ExtensionProcedureSymbol:
  """Extension factory for the -A models' BITROT, BITSHI calls."""
  assert bitrotate != bitshift  # Gotta have one of them, but just one
  name = 'BitRotate' if bitrotate else 'BitShift'

  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Statement:
    if len(args) != 3: raise Error(
        f'{name} parameters are an input string, a numeric argument, and an '
        'output string variable')

    # Check parameters.
    _check_expression_compatibility(t4050_types.String(), args[0])
    _check_expression_compatibility(
        mupas_types.IntegerSubrange(-65535, 65535), args[1])
    _check_expression_compatibility(
        _Constraints(t4050_types.String(), is_mutable_variable=True), args[2])
    typeinfo_in = args[0].typeinfo
    typeinfo_out = args[2].typeinfo
    assert isinstance(typeinfo_in, mupas_types.String)  # mypy...
    assert isinstance(typeinfo_out, mupas_types.String)
    if typeinfo_out.length < typeinfo_in.length: warnings.warn(
        f'In call to {name}: the destination string with maximum length '
        f'{typeinfo_out.length} might be asked to store a result string of '
        f'size {typeinfo_in.length}! Odd behaviour or program termination '
        'could result')

    # BITROT, but only BITROT and not BITSHI, can't have the same variable as
    # an input or an output argument. So we do a kludgy little check.
    if bitrotate and args[0].access == args[2].access: raise Error(
        f'In call to {name}: the destination string variable must not also be '
        "used as the input string; don't know why, Tektronix just says so")

    # Create statement result.
    call = 'BITROT' if bitrotate else 'BITSHI'
    code = args[0].compute + args[1].compute + [(
        f'      CAL "{name}",'
        f'{args[0].access},{args[1].access},{args[2].access}')]
    stack_growth = args[0].stack_growth + args[1].stack_growth
    return t4050_compiled.Statement(code=code, stack_growth=stack_growth)

  # Return the new extension.
  return t4050_extensions.ExtensionProcedureSymbol(extension=extension)


def _rowsum_or_columnsum(
    rowsum: bool = False, columnsum: bool = False,
) -> t4050_extensions.ExtensionProcedureSymbol:
  """Extension factory for the -A models' RSUM and CSUM commands."""
  assert rowsum != columnsum  # Gotta have one of them, but just one.
  name = 'RowSum' if rowsum else 'ColumnSum'

  def extension(
      args: list[t4050_compiled.Expression],
  ) -> t4050_compiled.Statement:
    if len(args) != 2: raise Error(
        f'{name} parameters are an input 2-D array and an output 1-D array')

    # Check parameters.
    _check_expression_compatibility(
         t4050_types.Array2d(None, None, mupas_types.ScalarNumber()), args[0])
    typeinfo_in = args[0].typeinfo
    assert isinstance(typeinfo_in, mupas_types.Array2d)  # mypy...

    _check_expression_compatibility(
        _Constraints(
            t4050_types.Array1d(None, typeinfo_in.value_typeinfo),
            is_mutable_variable=True), args[1])
    typeinfo_out = args[1].typeinfo
    assert isinstance(typeinfo_out, mupas_types.Array1d)

    rows_in, cols_in = array2d_dims(typeinfo_in)
    length_out = array1d_dims(typeinfo_out)
    output_dims_needed = rows_in if rowsum else cols_in
    if output_dims_needed != length_out: raise Error(
        f'In call to {name}: a {rows_in}x{cols_in} input array requires an '
        f'output vector of length {output_dims_needed}, not {length_out}')

    # Create statement result.
    command = 'RSUM' if rowsum else 'CSUM'
    code = args[0].compute + args[1].compute + [
        f'      {args[1].access}={command}({args[0].access})']
    stack_growth = args[0].stack_growth + args[1].stack_growth
    return t4050_compiled.Statement(code=code, stack_growth=stack_growth)

  # Return the new extension.
  return t4050_extensions.ExtensionProcedureSymbol(extension=extension)


#####################
### Other helpers ###
#####################


def _collect_io_address(
    name: str,
    param: t4050_compiled.Expression,
    want_io: bool,
    want_ioprimary: bool = False,
) -> tuple[str, str]:
  """Attempt to collect a 4050 BASIC I/O address from an expression.

  muPas programs for the 4050 BASIC target may sometimes specify 4050 BASIC
  I/O addresses in arguments to extensions. This must occur in string literal
  expressions (and not variables or in other forms). This function helps
  extract I/O addresses (e.g. '@12', '%12.34') from string literal expressions.

  Args:
    name: Name of the extension or language feature dealing with the I/O
        address (if present).
    param: Expression to analyse for the presence of an I/O address.
    want_io: Whether the caller wants a full I/O address if available. It's
        assumed that the caller can tolerate a primary-only address.
    want_ioprimary: Whether the caller wants exclusively a primary-only I/O
        address. If true, the presence of a secondary address will cause this
        function to raise an Error.

  Returns:
    ('', '') if want_io or want_ioprimary are false or if the expression does
    not appear to contain an I/O address. Otherwise, the first element is the
    I/O address in param with a trailing colon (e.g. '%98,76:', '@54:'), and
    the second is the same but omitting any secondary address (e.g. '%98:',
    '@54:').

  Raises:
    Error: as described.
  """
  io, ioprimary = '', ''
  if want_io or want_ioprimary:
    if m := _RE_IOADDRESS.fullmatch(param.access):
      io, ioprimary = f'{m.group(1)}:', f'{m.group(2)}:'
      # Make sure the user only supplies a primary address if that's all
      # that's desired by the extension.
      if want_ioprimary and io != ioprimary: raise Error(
          f'A full I/O address was supplied to {name}, but {name} does not '
          'accept secondary I/O addresses, only primary ones')

  return io, ioprimary


def _check_expression_compatibility(
    expected: mupas_types.Type, actual: t4050_compiled.Expression,
):
  # Carry out the extra checks that this function can perform, for expected
  # types wrapped in _Constraints objects.
  def constraint_fail():
    raise t4050_types.TypeMismatch(
        f'An expression of type {actual.typeinfo} failed to match the '
        f'constraint(s) <{expected}>')

  def size_warning(present: int, wanted: int):
    warnings.warn(
        f'A string or 1-D array that might need space for {wanted} elements '
        f'has only {present} elements: your program might crash!')

  match expected:
    case _Constraints():
      if (
          # Checks for values in mutable storage.
          (expected.can_be_lhs and not actual.can_be_lhs) or
          (expected.is_unqualified_variable and
               not actual.is_unqualified_variable) or
          # Kludgy access member pattern matching.
          (expected.access_matches is not None and
              not re.fullmatch(expected.access_matches, actual.access))):
        # Fail if any of these violations is discovered.
        constraint_fail()

      # Type-specific checks.
      match actual.typeinfo:
        # Strings.
        case mupas_types.String(length=length):
          if expected.has_room_for is not None:
            if length < expected.has_room_for: constraint_fail()
          if expected.might_want_room_for is not None:
            if length < expected.might_want_room_for: 
              size_warning(length, expected.might_want_room_for)
          if expected.even_number_of_elements:
            if length % 2: constraint_fail()

        # 1-D arrays.
        case mupas_types.Array1d(index_typeinfo=it, value_typeinfo=vt):
          size = array_num_elements(actual.typeinfo)
          if expected.has_room_for is not None:
            if size < expected.has_room_for: constraint_fail()
          if expected.might_want_room_for is not None:
            if size < expected.might_want_room_for: 
              size_warning(size, expected.might_want_room_for)
          if expected.even_number_of_elements is not None:
            if size % 2: constraint_fail()

        # 1-D arrays.
        case mupas_types.Array2d():
          size = array_num_elements(actual.typeinfo)
          if expected.even_number_of_elements is not None:
            if size % 2: constraint_fail()

  # Those extra checks have all passed, so now do the regular type
  # compatibility checking.
  t4050_types.check_assignment_or_parameter_compatibility(
      expected=expected, actual=actual.typeinfo)


def array1d_dims(typeinfo: mupas_types.Array1d) -> int:
  """Abbreviate retrieval of array dimensions."""
  return (typeinfo.index_typeinfo.upper_bound -
          typeinfo.index_typeinfo.lower_bound) + 1


def array2d_dims(typeinfo: mupas_types.Array2d) -> tuple[int, int]:
  """Abbreviate retrieval of array dimensions."""
  rows = (typeinfo.row_index_typeinfo.upper_bound -
          typeinfo.row_index_typeinfo.lower_bound) + 1
  cols = (typeinfo.col_index_typeinfo.upper_bound -
          typeinfo.col_index_typeinfo.lower_bound) + 1
  return rows, cols


def array_num_elements(
    typeinfo: mupas_types.Array1d | mupas_types.Array2d
) -> int:
  """Number of elements in any kind of array."""
  match typeinfo:
    case mupas_types.Array1d():
      return array1d_dims(typeinfo)
    case mupas_types.Array2d():
      rows, cols = array2d_dims(typeinfo)
      return rows * cols


class Error(RuntimeError):
  """Custom errors for `_extension_function` and `_extension_procedure`.

  Dedicated errors make the `_try_several...` routines easier to implement, as
  they make it easy to detect the kind of error that means "give up and try
  the next extension.
  """
