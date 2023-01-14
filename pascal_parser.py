"""Lisa Pascal/Clascal parser.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This parser uses the grammar in `pascal_grammar.lark` to derive an abstract
syntax tree for a Pascal/Clascal program text supplied in a (long) string. The
string must have already been preprocessed by the preprocessor defined in
(where else) the `preprocessor` module.

Abstract syntax tree nodes are instances of the Python dataclasses defined
in the first half of this file. The second half assembles a Lark Transformer
class that transforms the Lark parser's own tree nodes into the dataclasses
defined in the first half. Our own nodes are slightly friendlier to work with
during code generation than Lark's, or at least so claims this docstring.

At the bottom of the module is some rudimentary infrastructure for aiding
pretty-printing: this may be useful for extending the parser or for
implementing code generators for different architectures.
"""

import dataclasses
import enum
import functools
import itertools

import lark

from typing import Optional, Union


def parse(
    preprocessed_program_text: str,
) -> Union['Program', 'Unit']:
  """Parse a preprocessed *ascal source file.

  Args:
    preprocessed_program_text: Preprocessed program text, as returned by the
        `preprocess` function in the `preprocessor` module.

  Returns:
    An abstract syntax tree for the preprocessed program text.
  """
  return _Transformer().transform(_parser().parse(preprocessed_program_text))


@functools.cache
def _parser() -> lark.Lark:
  """Create/retrieve a singleton Lark parser from the language grammar."""
  return lark.Lark.open('pascal_grammar.lark', maybe_placeholders=True)


### Generic AST node, for type checking.

@dataclasses.dataclass
class AstNode:
  """Base class for all parse tree nodes."""


### _Transformer

@dataclasses.dataclass
class Block(AstNode):
  """Node for blocks."""
  label_declaration_part: Optional['LabelDeclarationPart']
  constant_declaration_part: Optional['ConstantDeclarationPart']
  type_declaration_part: Optional['TypeDeclarationPart']
  variable_declaration_part: Optional['VariableDeclarationPart']
  procedure_and_function_definition_part: Optional[
      'ProcedureAndFunctionDefinitionPart']
  statement_part: 'StatementPart'

@dataclasses.dataclass
class Identifier(AstNode):
  """Leaf node for identifiers."""
  text: str

# TODO: Move somewhere else.
@dataclasses.dataclass
class NameAndType(AstNode):
  """Node that associates a type to a name."""
  text: str
  the_type: 'Type'


### _TransformerProgramAndUnitMixin

@dataclasses.dataclass
class InterfacePart(AstNode):
  """Node for the interface parts of units."""
  uses_clause: Optional['UsesClause']
  constant_declaration_part: Optional['ConstantDeclarationPart']
  type_declaration_part: Optional['TypeDeclarationPart']
  variable_declaration_part: Optional['VariableDeclarationPart']
  procedure_and_function_declaration_part: Optional[
      'ProcedureAndFunctionDeclarationPart']

@dataclasses.dataclass
class ImplementationPart(AstNode):
  """Node for the implementation parts of units."""
  constant_declaration_part: Optional['ConstantDeclarationPart']
  type_declaration_part: Optional['TypeDeclarationPart']
  variable_declaration_part: Optional['VariableDeclarationPart']
  subroutine_part: Optional['SubroutinePart']

@dataclasses.dataclass
class Intrinsic(AstNode):
  """Leaf node marking intrinsic non-shared libraries."""

@dataclasses.dataclass
class IntrinsicShared(Intrinsic):
  """Leaf node marking intrinsic shared libraries."""

@dataclasses.dataclass
class Program(AstNode):
  """Node for programs."""
  text: str
  parameters: Optional[tuple[str]]
  uses_clause: Optional['UsesClause']
  block: Block

@dataclasses.dataclass
class Unit(AstNode):
  """Node for units."""
  text: str
  is_intrinsic: bool
  is_shared: bool
  interface_part: InterfacePart
  implementation_part: ImplementationPart

@dataclasses.dataclass
class UsesClause(AstNode):
  """Uses clause: a list of identifiers indicating modules in use."""
  module_identifiers: tuple[str, ...]


### _TransformerLabelPartMixin

@dataclasses.dataclass
class LabelDeclarationPart(AstNode):
  """A collection of label declarations."""
  labels: tuple[str, ...]


### _TransformerConstPartMixin

@dataclasses.dataclass
class Constant(AstNode):
  """Base class for constants."""

@dataclasses.dataclass
class ConstantIdentifier(Constant):
  """Identifier of a constant, possibly negated."""
  negated: bool
  text: str

@dataclasses.dataclass
class ConstantQuotedConstant(Constant):
  """A quoted constant."""
  index: int        # Index into the list of quoted constants.
  code_length: int  # Number of chars used by the whole constant in source code.

@dataclasses.dataclass
class ConstantSignedInteger(Constant):
  """A signed integer constant."""
  number: int

@dataclasses.dataclass
class ConstantSignedReal(Constant):
  """A signed real constant."""
  number: float

@dataclasses.dataclass
class ConstantDeclaration(AstNode):
  """A leaf node for a constant declaration."""
  text: str
  value: Constant

@dataclasses.dataclass
class ConstantDeclarationPart(AstNode):
  """A collection of constant declarations."""
  declarations: tuple[ConstantDeclaration, ...]


### _TransformerTypePartMixin

@dataclasses.dataclass
class TypeDeclarationPart(AstNode):
  """A sequence of type declarations."""
  types: tuple[str, ...]


### _TransformerVarPartMixin

@dataclasses.dataclass
class VariableDeclarationPart(AstNode):
  """A sequence of variable declarations."""
  declarations: tuple[NameAndType, ...]


### _TransformerSubroutinePartMixin

@dataclasses.dataclass
class MethodBlock(AstNode):
  """Class method definitions."""
  class_identifier: str
  methods: tuple['SubroutineDefinition', ...]
  creation_block: Optional[Block]

@dataclasses.dataclass
class ProcedureAndFunctionDeclarationPart(AstNode):
  """A sequence of procedure and function declarations."""
  subroutines: tuple['SubroutineDeclaration', ...]

@dataclasses.dataclass
class ProcedureAndFunctionDefinitionPart(AstNode):
  """A sequence of procedure and function definitions."""
  subroutines: tuple['SubroutineDefinition', ...]

@dataclasses.dataclass
class SubroutinePart(AstNode):
  """A sequence of subroutine definitions."""
  subroutines: tuple['SubroutineDefinition', ...]


### _TransformerStatementPartMixin

@dataclasses.dataclass
class StatementPart(AstNode):
  """A sequence of statements."""
  statements: 'StatementCompound'


### _TransformerRecordAndClassTypeMixin

@dataclasses.dataclass
class TypeRecordFixedPart(AstNode):
  """Node for the fixed part of record types."""
  fields: tuple[NameAndType, ...]

@dataclasses.dataclass
class TypeRecordVariant(AstNode):
  """Node for individual variants in record types."""
  constants: tuple[Constant, ...]
  fields: Optional['TypeRecordFields']

@dataclasses.dataclass
class TypeRecordVariantPart(AstNode):
  tag_identifier: Optional[Identifier]
  tag_field_type: 'Type'
  variants: tuple[TypeRecordVariant, ...]

@dataclasses.dataclass
class TypeRecordFields(AstNode):
  """Node for all of the fields of a record."""
  fixed_part: Optional[TypeRecordFixedPart]
  variant_part: Optional[TypeRecordVariantPart]

@dataclasses.dataclass
class TypeRecord(AstNode):
  """Node for a record type declaration."""
  packed: bool
  fields: Optional[TypeRecordFields]

@dataclasses.dataclass
class TypeClass(TypeRecord):
  """Node for class declarations."""
  class_identifier: str
  methods: tuple[Union['MethodProcedureHeading', 'MethodFunctionHeading'], ...]


### _TransformerSubroutineDefinitionMixin

@dataclasses.dataclass
class SubroutineDeclaration(AstNode):
  """Base class for subroutine declarations."""

@dataclasses.dataclass
class SubroutineDefinition(AstNode):
  """Base class for subroutine definitions."""

@dataclasses.dataclass
class FormalParameterList(AstNode):
  """Sequence of name/type pairs used to specify function parameters."""
  parameters: tuple['ParameterDeclaration']

@dataclasses.dataclass
class FunctionDeclaration(SubroutineDeclaration):
  """Node for function declarations."""
  heading: 'FunctionHeading'

@dataclasses.dataclass
class FunctionDefinition(SubroutineDefinition):
  """Node for function definitions."""
  heading: 'FunctionHeading'
  body: 'SubroutineBody'

@dataclasses.dataclass
class FunctionHeading(AstNode):
  """Node for function names, parameters, and return types."""
  text: str
  parameters: tuple['ParameterDeclaration']
  result_type: Optional['Type']  # Optional for external declarations.

@dataclasses.dataclass
class ParameterDeclaration(AstNode):
  """Node for individual function and procedure parameters."""
  by_reference: bool  # If false, then by value
  text: str
  the_type: 'Type'

@dataclasses.dataclass
class ProcedureDeclaration(SubroutineDeclaration):
  """Node for procedure declarations."""
  heading: 'ProcedureHeading'

@dataclasses.dataclass
class ProcedureDefinition(SubroutineDefinition):
  """Node for procedure definitions."""
  heading: 'ProcedureHeading'
  body: 'SubroutineBody'

@dataclasses.dataclass
class ProcedureHeading(AstNode):
  """Node for procedure names, parameters, and return types."""
  text: str
  parameters: tuple['ParameterDeclaration']

@dataclasses.dataclass
class SubroutineBody(AstNode):
  """Base class for subroutine bodies."""

@dataclasses.dataclass
class SubroutineBodyBlock(SubroutineBody):
  """Leaf node for locally-defined subroutines."""
  block: Block

@dataclasses.dataclass
class SubroutineBodyExternal(SubroutineBody):
  """Leaf node for declarations of external subroutines."""

@dataclasses.dataclass
class SubroutineBodyForward(SubroutineBody):
  """Leaf node for forward declarations of subroutines."""

@dataclasses.dataclass
class MethodFunctionHeading(FunctionHeading):
  class_identifier: str

@dataclasses.dataclass
class MethodProcedureHeading(ProcedureHeading):
  class_identifier: str


### _TransformerQualifiedBindingReferenceOrCallMixin

@dataclasses.dataclass
class BindingReferenceOrCall(AstNode):
  """Sequence of expressions used as function parameters."""
  binding: str
  qualifiers_and_parameters: tuple[Union['ParameterList', 'Qualifier'], ...]

@dataclasses.dataclass
class ParameterList(AstNode):
  """Sequence of expressions used as function parameters."""
  parameters: tuple['Expression']

@dataclasses.dataclass
class Qualifier(AstNode):
  """Base class for variable qualifiers."""

@dataclasses.dataclass
class QualifierFieldDesignator(Qualifier):
  """Leaf node for field names."""
  text: str

@dataclasses.dataclass
class QualifierDereferenceOrRead(Qualifier):
  """Marker for the Pascal postfix ^ operator."""

@dataclasses.dataclass
class QualifierIndex(Qualifier):
  """Sequence of expressions used as array indices."""
  indices: tuple['Expression', ...]


### _TransformerExpressionMixin

class BinaryOp(enum.Enum):
  """Binary operations for ExpressionBinary."""
  MULTIPLY = 1
  DIVIDE_TO_REAL = 2
  DIVIDE_TO_INT = 3
  MODULO = 4
  LOGICAL_AND = 5
  ADD = 6
  SUBTRACT = 7
  LOGICAL_OR = 8
  COMPARE_EQ = 9
  COMPARE_LT = 10
  COMPARE_GT = 11
  COMPARE_LE = 12
  COMPARE_GE = 13
  COMPARE_NE = 14
  IS_IN_SET = 15

class UnaryOp(enum.Enum):
  """Unary operations for ExpressionUnary."""
  ADDRESS_OF = 1
  LOGICAL_NOT = 2
  NEGATE = 3

@dataclasses.dataclass
class Expression(AstNode):
  """Base class for expressions."""

@dataclasses.dataclass
class ExpressionBinary(Expression):
  """Node for a binary operation."""
  op: BinaryOp
  expression_left: Expression
  expression_right: Expression

@dataclasses.dataclass
class ExpressionLeaf(Expression):
  """Node for the terminals of expression trees."""
  value: Union['UnsignedInteger', 'UnsignedReal', 'QuotedConstant', 'Nil',
               BindingReferenceOrCall]

@dataclasses.dataclass
class ExpressionSet(Expression):
  """Node for set expressions."""
  members: tuple['ExpressionSetMembers']

@dataclasses.dataclass
class ExpressionSetMembers(AstNode):
  """Base class for kinds of elements in a set expression."""

@dataclasses.dataclass
class ExpressionSetRange(AstNode):
  """A range of items in a set expression."""
  expression_left: Expression
  expression_right: Expression

@dataclasses.dataclass
class ExpressionSetSingleItem(AstNode):
  """Individual item in a set expression."""
  item: Expression

@dataclasses.dataclass
class ExpressionUnary(Expression):
  """Node for a unary operation."""
  op: UnaryOp
  expression: Expression


### _TransformerStatementMixin

@dataclasses.dataclass
class CaseClause(AstNode):
  """Node for clauses in case statements."""
  constants: tuple[Constant, ...]
  consequent: 'Statement'

@dataclasses.dataclass
class Statement(AstNode):
  """Base class for statements."""

@dataclasses.dataclass
class LabelAndStatement(AstNode):
  """Paired labels and statements."""
  label: Optional[int]
  statement: Optional['Statement']

@dataclasses.dataclass
class StatementAssignment(Statement):
  """Node for assignment statements."""
  destination: BindingReferenceOrCall
  value: Expression

@dataclasses.dataclass
class StatementCase(Statement):
  """Node for case statements."""
  selector: Expression
  cases: tuple[CaseClause, ...]
  otherwise: Optional[Statement]

@dataclasses.dataclass
class StatementCompound(Statement):
  """Node for compound statements (statement sequences)."""
  statements: tuple[Statement, ...]

@dataclasses.dataclass
class StatementFor(Statement):
  """Node for for statements."""
  control_variable: str
  initial_value: Expression
  step: int
  final_value: Expression
  body: Statement

@dataclasses.dataclass
class StatementGoto(Statement):
  """Node for goto statements."""
  label: int

@dataclasses.dataclass
class StatementIf(Statement):
  """Node for if statements."""
  condition: Expression
  consequent: Statement
  alternative: Optional[Statement]

@dataclasses.dataclass
class StatementProcedure(Statement):
  """Node for procedure calls."""
  call: BindingReferenceOrCall

@dataclasses.dataclass
class StatementRepeat(Statement):
  """Node for repeat statements."""
  body: tuple[Statement, ...]
  condition: Expression

@dataclasses.dataclass
class StatementWhile(Statement):
  """Node for while statements."""
  condition: Expression
  body: Statement

@dataclasses.dataclass
class StatementWith(Statement):
  """Node for with statements."""
  record_variables: tuple[BindingReferenceOrCall, ...]
  body: Statement


### _TransformerTypeMixin

@dataclasses.dataclass
class Type(AstNode):
  """Base class for types."""

@dataclasses.dataclass
class TypeArray(Type):
  """Node for Array types."""
  packed: bool
  index_types: tuple[Type, ...]
  value_type: Type

@dataclasses.dataclass
class TypeBoolean(Type):
  """Leaf node for Boolean types."""

@dataclasses.dataclass
class TypeChar(Type):
  """Leaf node for Char types."""

@dataclasses.dataclass
class TypeEnumerated(Type):
  """Leaf node for enumerated types."""
  value_identifiers: tuple[str, ...]

@dataclasses.dataclass
class TypeFile(Type):
  """Node for file objects."""
  packed: bool
  data_type: Optional[Type]

@dataclasses.dataclass
class TypeIdentifier(Type):
  """Leaf node for Integer types."""
  text: str

@dataclasses.dataclass
class TypeInteger(Type):
  """Leaf node for Integer types."""

@dataclasses.dataclass
class TypeLongint(Type):
  """Leaf node for Longint types."""

@dataclasses.dataclass
class TypePointer(Type):
  """Leaf node for Pointer types."""
  base_type: Type

@dataclasses.dataclass
class TypeReal(Type):
  """Leaf node for Real types."""

@dataclasses.dataclass
class TypeSet(Type):
  """Node for Set types."""
  packed: bool
  base_type: Type

@dataclasses.dataclass
class TypeString(Type):
  """Leaf node for String types."""
  stringsize: int

@dataclasses.dataclass
class TypeSubrange(Type):
  """Leaf node for subrange types."""
  lower_bound: Constant
  upper_bound: Constant


# _TransformerLiteralMixin

@dataclasses.dataclass
class DigitSequence(AstNode):
  """Leaf node for digit sequences."""
  text: str
  number: int

@dataclasses.dataclass
class Nil(AstNode):
  pass

@dataclasses.dataclass
class QuotedConstant(AstNode):
  """Leaf node for quoted constants."""
  index: int        # Index into the list of quoted constants.
  code_length: int  # Number of chars used by the whole constant in source code.

@dataclasses.dataclass
class SignedInteger(AstNode):
  """Leaf node for signed integers."""
  number: int

@dataclasses.dataclass
class SignedReal(AstNode):
  """Leaf node for signed real numbers."""
  number: float

@dataclasses.dataclass
class UnsignedInteger(AstNode):
  """Leaf node for unsigned integers."""
  number: int

@dataclasses.dataclass
class UnsignedReal(AstNode):
  """Leaf node for unsigned real numbers."""
  number: float


######################
#### TRANSFORMERS ####
######################


class _TransformerProgramAndUnitMixin:

  interface_part = lark.v_args(inline=True)(InterfacePart)

  intrinsic = lark.v_args(inline=True)(Intrinsic)

  intrinsic_shared = lark.v_args(inline=True)(IntrinsicShared)

  implementation_part = lark.v_args(inline=True)(ImplementationPart)

  @lark.v_args(inline=True)
  def program(self, identifier, parameters, uses_clause, block):
    return Program(identifier.text,
                   parameters,
                   uses_clause,
                   block)

  @lark.v_args(inline=True)
  def unit(self, name, intrinsic, interface_part, implementation_part):
    return Unit(text=name.text,
                is_intrinsic=intrinsic is not None,
                is_shared=isinstance(intrinsic, IntrinsicShared),
                interface_part=interface_part,
                implementation_part=implementation_part)

  uses_clause = lark.v_args(inline=True)(UsesClause)


class _TransformerLabelPartMixin:

  def label_declaration_part(self, items):
    return LabelDeclarationPart(tuple(i.text for i in items))


class _TransformerConstPartMixin:

  @lark.v_args(inline=True)
  def constant(self, value):
    match value:
      case QuotedConstant(index=index, code_length=code_length):
        return ConstantQuotedConstant(index, code_length)
      case SignedReal(number=number):
        return ConstantSignedReal(number)
      case SignedInteger(number=number):
        return ConstantSignedInteger(number)
      case ConstantIdentifier():
        return value
      case _: raise _InternalError

  @lark.v_args(inline=True)
  def constant_declaration(self, identifier, constant):
    return ConstantDeclaration(identifier.text, constant)

  constant_declarations = tuple

  constant_declaration_part = lark.v_args(inline=True)(ConstantDeclarationPart)

  @lark.v_args(inline=True)
  def signed_constant_identifier(self, sign, identifier):
    return ConstantIdentifier(
        negated=(sign and sign.data == 'sign_minus'), text=identifier.text)


class _TransformerTypePartMixin:

  @lark.v_args(inline=True)
  def type_declaration(self, identifier, the_type):
    return NameAndType(identifier.text, the_type)

  type_declarations = tuple

  type_declaration_part = lark.v_args(inline=True)(TypeDeclarationPart)

  @lark.v_args(inline=True)
  def type_identifier(self, identifier):
    return TypeIdentifier(identifier.text)


class _TransformerVarPartMixin:

  @lark.v_args(inline=True)
  def variable_declaration(self, identifiers, the_type):
    return tuple(NameAndType(ident, the_type) for ident in identifiers)

  def variable_declarations(self, items):
    return tuple(itertools.chain(*items))

  variable_declaration_part = lark.v_args(inline=True)(VariableDeclarationPart)


class _TransformerSubroutinePartMixin:

  @lark.v_args(inline=True)
  def method_block(self, class_identifier, methods, creation_block):
    return MethodBlock(class_identifier.text, methods, creation_block)

  procedure_and_function_declaration_part = lark.v_args(inline=True)(
      ProcedureAndFunctionDeclarationPart)

  procedure_and_function_declarations = tuple

  procedure_and_function_definition_part = lark.v_args(inline=True)(
      ProcedureAndFunctionDefinitionPart)

  procedure_and_function_definitions = tuple

  subroutine_part = lark.v_args(inline=True)(SubroutinePart)

  subroutines = tuple


class _TransformerStatementPartMixin:

  statement_part = lark.v_args(inline=True)(StatementPart)


class _TransformerRecordAndClassTypeMixin:

  @lark.v_args(inline=True)
  def class_type(self, class_identifier, fields, methods):
    return TypeClass(packed=False, fields=fields,
                     class_identifier=class_identifier.text, methods=methods)

  @lark.v_args(inline=True)
  def field_declaration(self, identifiers, the_type):
    return tuple(NameAndType(ident, the_type) for ident in identifiers)

  @lark.v_args(inline=True)
  def field_list_with_fixed_part(self, fixed_part):
    return TypeRecordFields(fixed_part, None)

  field_list_with_fixed_and_variant_parts = (
      lark.v_args(inline=True)(TypeRecordFields))

  @lark.v_args(inline=True)
  def field_list_with_variant_part(self, variant_part):
    return TypeRecordFields(None, variant_part)

  def fixed_part(self, items):
    return TypeRecordFixedPart(tuple(itertools.chain(*items)))

  method_interface = tuple

  @lark.v_args(inline=True)
  def record_type(self, fields):
    return TypeRecord(packed=False, fields=fields)

  variant = lark.v_args(inline=True)(TypeRecordVariant)

  variant_constants = tuple

  @lark.v_args(inline=True)
  def variant_part(self, tag_identifier, tag_field_type, *variants):
    return TypeRecordVariantPart(tag_identifier, tag_field_type, variants)


class _TransformerSubroutineDefinitionMixin:

  def formal_parameters(self, items):
    return tuple(itertools.chain(*items))

  formal_parameter_list = lark.v_args(inline=True)(FormalParameterList)

  function_declaration = lark.v_args(inline=True)(FunctionDeclaration)

  function_definition = lark.v_args(inline=True)(FunctionDefinition)

  @lark.v_args(inline=True)
  def function_heading(self, identifier, parameters, result_type):
    return FunctionHeading(identifier.text, parameters, result_type)

  @lark.v_args(inline=True)
  def method_function_heading(
      self, class_identifier, identifier, parameters, result_type):
    return MethodFunctionHeading(
        identifier.text, parameters, result_type, class_identifier.text)

  @lark.v_args(inline=True)
  def method_procedure_heading(self, class_identifier, identifier, parameters):
    return MethodProcedureHeading(
        identifier.text, parameters, class_identifier.text)

  @lark.v_args(inline=True)
  def parameter_declaration(self, var, identifiers, the_type):
    return tuple(
        ParameterDeclaration(
            by_reference=var is not None, text=ident, the_type=the_type)
        for ident in identifiers)

  parameter_subroutine = tuple

  procedure_declaration = lark.v_args(inline=True)(ProcedureDeclaration)

  procedure_definition = lark.v_args(inline=True)(ProcedureDefinition)

  @lark.v_args(inline=True)
  def procedure_heading(self, identifier, parameters):
    return ProcedureHeading(identifier.text, parameters)

  subroutine_body_block = lark.v_args(inline=True)(SubroutineBodyBlock)

  def subroutine_body_external(self, items):
    return SubroutineBodyExternal()

  def subroutine_body_forward(self, items):
    return SubroutineBodyForward()


class _TransformerQualifiedBindingReferenceOrCallMixin:

  actual_parameters = tuple

  actual_parameter_list = lark.v_args(inline=True)(ParameterList)

  @lark.v_args(inline=True)
  def dereference_or_read_symbol(self):
    return QualifierDereferenceOrRead()

  @lark.v_args(inline=True)
  def field_designator(self, identifier):
    return QualifierFieldDesignator(identifier.text)

  index = lark.v_args(inline=True)(QualifierIndex)

  indices = tuple

  @lark.v_args(inline=True)
  def qualified_binding_reference_or_call(
      self, binding, *qualifiers_and_parameters):
    return BindingReferenceOrCall(binding.text, qualifiers_and_parameters)


class _TransformerExpressionMixin:

  @lark.v_args(inline=True)
  def expression_binary(self, expression_left, exp_op, expression_right):
    op = {'infix_equal': BinaryOp.COMPARE_EQ,
          'infix_lt': BinaryOp.COMPARE_LT,
          'infix_gt': BinaryOp.COMPARE_GT,
          'infix_le': BinaryOp.COMPARE_LE,
          'infix_ge': BinaryOp.COMPARE_GE,
          'infix_ne': BinaryOp.COMPARE_NE,
          'infix_in': BinaryOp.IS_IN_SET}[exp_op.data]
    return ExpressionBinary(op, expression_left, expression_right)

  @lark.v_args(inline=True)
  def factor_address_of(self, factor):
    return ExpressionUnary(UnaryOp.ADDRESS_OF, factor)

  @lark.v_args(inline=True)
  def factor_not(self, factor):
    return ExpressionUnary(UnaryOp.LOGICAL_NOT, factor)

  factor_qualified_binding_reference_or_call = (
      lark.v_args(inline=True)(ExpressionLeaf))

  factor_set_constructor = lark.v_args(inline=True)(ExpressionSet)

  factor_unsigned_constant = lark.v_args(inline=True)(ExpressionLeaf)

  member_group_range = lark.v_args(inline=True)(ExpressionSetRange)

  member_group_single_item = lark.v_args(inline=True)(ExpressionSetSingleItem)

  member_groups = tuple

  @lark.v_args(inline=True)
  def simple_expression_with_sign(self, sign, expression):
    def _negate_leftmost_term(exp):
      if (isinstance(exp, ExpressionBinary) and
          exp.op in [BinaryOp.ADD, BinaryOp.SUBTRACT, BinaryOp.LOGICAL_OR]):
        return ExpressionBinary(exp.op,
                                _negate_leftmost_term(exp.expression_left),
                                exp.expression_right)
      else:
        return ExpressionUnary(UnaryOp.NEGATE, exp)

    if sign.data == 'sign_minus':
      return _negate_leftmost_term(expression)
    else:
      return expression

  @lark.v_args(inline=True)
  def simple_expression_no_sign_binary(self, left_exp, exp_op, right_term):
    op = {'infix_plus': BinaryOp.ADD,
          'infix_minus': BinaryOp.SUBTRACT,
          'infix_or': BinaryOp.LOGICAL_OR}[exp_op.data]
    return ExpressionBinary(op, left_exp, right_term)

  @lark.v_args(inline=True)
  def term_binary(self, left_term, term_op, right_factor):
    op = {'infix_asterisk': BinaryOp.MULTIPLY,
          'infix_slash': BinaryOp.DIVIDE_TO_REAL,
          'infix_div': BinaryOp.DIVIDE_TO_INT,
          'infix_mod': BinaryOp.MODULO,
          'infix_and': BinaryOp.LOGICAL_AND}[term_op.data]
    return ExpressionBinary(op, left_term, right_factor)


class _TransformerStatementMixin:

  assignment_statement = lark.v_args(inline=True)(StatementAssignment)

  def case(self, items):
    return CaseClause(constants=tuple(items[:-1]), consequent=items[-1])

  cases = tuple

  case_statement = lark.v_args(inline=True)(StatementCase)

  compound_statement = lark.v_args(inline=True)(StatementCompound)

  if_statement = lark.v_args(inline=True)(StatementIf)

  @lark.v_args(inline=True)
  def for_statement(
      self, control_variable, initial_value, direction, final_value, body):
    return StatementFor(
        control_variable.text,
        initial_value,
        1 if direction.data == 'to' else -1,
        final_value,
        body)

  @lark.v_args(inline=True)
  def for_direction(self, direction: lark.Tree) -> int:
    return 1 if direction.data == 'to' else -1

  @lark.v_args(inline=True)
  def goto_statement(self, label: DigitSequence) -> StatementGoto:
    return StatementGoto(label.number)

  procedure_statement = lark.v_args(inline=True)(StatementProcedure)

  record_variable_references = tuple

  repeat_statement = lark.v_args(inline=True)(StatementRepeat)

  @lark.v_args(inline=True)
  def statement(
      self, label: Optional[DigitSequence], statement: Statement
  ) -> LabelAndStatement:
    return LabelAndStatement(None if label is None else label.number, statement)

  def statements(self, stmts):
    return tuple(s for s in stmts if s.label or s.statement)

  while_statement = lark.v_args(inline=True)(StatementWhile)

  with_statement = lark.v_args(inline=True)(StatementWith)


class _TransformerTypeMixin(_TransformerRecordAndClassTypeMixin):

  def array_type(self, items):
    return TypeArray(
        packed=False,
        index_types=tuple(items[:-1]),
        value_type=items[-1])

  def boolean_type_identifier(self, items):
    return TypeBoolean()

  def char_type_identifier(self, items):
    return TypeChar()

  enumerated_type = lark.v_args(inline=True)(TypeEnumerated)

  @lark.v_args(inline=True)
  def file_type(self, data_type):
    return TypeFile(packed=False, data_type=data_type)

  def identifier_list(self, ids_list):
    return tuple(ident.text for ident in ids_list)

  def integer_type_identifier(self, items):
    return TypeInteger()

  def longint_type_identifier(self, items):
    return TypeLongint()

  pointer_type = lark.v_args(inline=True)(TypePointer)

  def real_type_identifier(self, items):
    return TypeReal()

  @lark.v_args(inline=True)
  def structured_type(self, packed, the_type):
    the_type.packed = bool(packed and packed.data == 'packed')
    return the_type

  subrange_type = lark.v_args(inline=True)(TypeSubrange)

  @lark.v_args(inline=True)
  def set_type(self, base_type):
    return TypeSet(packed=False, base_type=base_type)

  @lark.v_args(inline=True)
  def string_type(self, size_attribute):
    return TypeString(size_attribute.number)


class _TransformerLiteralMixin:

  def DIGIT_SEQUENCE(self, items):
    text = ''.join(items)
    return DigitSequence(text, int(text))

  def HEX_DIGIT_SEQUENCE(self, items):
    text = ''.join(items)
    return DigitSequence(text, int(text, base=16))

  def nil(self, items):
    return Nil()

  def quoted_constant(self, items):
    index, code_length = items
    return QuotedConstant(index.number, code_length.number)

  @lark.v_args(inline=True)
  def scale_factor(self, sign, digits):
    sign_text = '-' if sign and sign.data == 'sign_minus' else '+'
    return f'e{sign_text}{digits.text}'

  @lark.v_args(inline=True)
  def signed_number(self, sign, number):
    multiplier = -1 if sign and sign.data == 'sign_minus' else 1
    match number:
      case UnsignedInteger(number=the_number):
        return SignedInteger(multiplier * the_number)
      case UnsignedReal(number=the_number):
        return SignedReal(multiplier * the_number)
      case _: raise _InternalError

  @lark.v_args(inline=True)
  def unsigned_integer(self, number):
    return UnsignedInteger(number.number)

  @lark.v_args(inline=True)
  def unsigned_number(self, number):
    return number

  @lark.v_args(inline=True)
  def unsigned_real(self, value):
    return value

  @lark.v_args(inline=True)
  def unsigned_real_with_fractional_part(
      self, integer_part, fractional_part, scale_factor):
    scale_factor = scale_factor or ''
    return UnsignedReal(float(
        f'{integer_part.text}.{fractional_part.text}{scale_factor}'))

  @lark.v_args(inline=True)
  def unsigned_real_without_fractional_part(self, integer_part, scale_factor):
    return UnsignedReal(float(f'{integer_part.text}{scale_factor}'))


class _Transformer(_TransformerProgramAndUnitMixin,
                   _TransformerLabelPartMixin,
                   _TransformerConstPartMixin,
                   _TransformerTypePartMixin,
                   _TransformerVarPartMixin,
                   _TransformerSubroutinePartMixin,
                   _TransformerStatementPartMixin,
                   _TransformerSubroutineDefinitionMixin,
                   _TransformerQualifiedBindingReferenceOrCallMixin,
                   _TransformerExpressionMixin,
                   _TransformerStatementMixin,
                   _TransformerTypeMixin,
                   _TransformerLiteralMixin,
                   lark.Transformer):

  block = lark.v_args(inline=True)(Block)

  def IDENTIFIER(self, items):
    return Identifier(''.join(items))

  @lark.v_args(inline=True)
  def start(self, program_or_unit):
    return program_or_unit


@dataclasses.dataclass
class Extension(AstNode):
  """Marks target-specific extensions, which aren't defined in code.

  This AST node is present as a convenience to compilers and interpreters that
  might be using this parser and that have data structures for symbol bindings
  that need to refer to an AST node. A symbol referring to an extension won't
  have a true corresponding node in the AST, so a dummy AST node made from an
  instance of this class can work as a stand-in.

  No AST node of this type will ever be found in an AST created by the `parse`
  function.
  """


#######################
#### ODDS AND ENDS ####
#######################


class _InternalError(RuntimeError):
  """An uninformative exception for "this shouldn't happen" errors."""


@dataclasses.dataclass
class Colour:
  """For coding and debugging: text that prints in colour.

  Uses ANSI escape codes to colour text; your terminal must support them.

  Attributes:
    colour_id: A numerical string that identifies a 3-bit or 4-bit foreground
        or background colour --- refer to the table at
        https://en.wikipedia.org/w/index.php?title=ANSI_escape_code&oldid=1128711850#3-bit_and_4-bit
    item: Text to print in colour.
  """
  colour_id: str
  item: str
  def __repr__(self):
    return f'\033[{self.colour_id}m{self.item}\033[0m'


def asdict_rec(ast):
  """For parser coding and debugging: make a printable abstract syntax tree.

  This quick hack transforms an abstract syntax tree into a structure made of
  easily-(pretty-)printable nests of Python built-in types: the main advantage
  here is that this nest will pretty-print (slightly) more compactly than the
  original AST, plus with colour (see below). One downside: dataclasses (like
  most of the AST notes) become dicts, which makes the ordering of their
  members arbitrary.

  Uses ANSI escape codes to colour the names of AST node types; your terminal
  must support them. Type names in green are AST node types defined in this
  file; names on a red background are Lark parse tree nodes and still need to
  be transformed --- time to write more transformer code in this module!

  Recommended usage:
     import pprint
     pprint.pprint(asdict_rec(parse(my_code)))

  Args:
    ast: An abstract syntax tree.

  Returns:
    A pretty-printable data structure as described.
  """
  match ast:
    case tuple():
      return tuple(asdict_rec(item) for item in ast)
    case list():
      return list(asdict_rec(item) for item in ast)
    case str():
      return ast
    case lark.Tree(data=data, children=children):
      return Colour('41', data), asdict_rec(children)
  if dataclasses.is_dataclass(ast):
    name = Colour('92', type(ast).__name__)
    items = ast.__dict__.items()
    if items:
      return name, {k: asdict_rec(v) for k, v in ast.__dict__.items()}
    else:
      return name
  else:
    return ast
