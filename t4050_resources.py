r"""muPas static/stack storage resources for the Tektronix 4050 BASIC target.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

This module defines `mupas_stack.[...]Allocator`, `mupas_stack.Frame`, and
`mupas_static.StaticAllocator` classes for the Tektronix 4050-series BASIC
target.

**4050 BASIC static resources** make direct use of BASIC string and numeric
variables. This target-specific decision about where to keep that data is
actually made in the `mupas_analyses` module: it probably should be moved into
this module or one close by.

**4050 BASIC stack resources** make use of one numeric array variable and two
numeric scalar variables acting as a stack, a stack pointer, and a frame
pointer respectively. In contrast to the "abstracted muPas stack frame"
described in mupas_stack.py, the stack frames used by the 4050 BASIC target
take this form:

     [ Return value (if a function)  ]   <Positive, incrementing indices>
     [ Old frame pointer             ]
     [ Pointer to enclosing scope    ]            |  Direction of stack growth
     [ Subroutine scalar parameter 1 ] <-- FP     |
     [ Subroutine scalar parameter 2 ]          \ | /
     [ ...                           ]           \|/
     [ Local scalar variable 1       ]            v
     [ Local scalar variable 2       ]
     [ ...                           ]
     [ First unused stack location   ] <-- SP

Note the absence of a return address: 4050 BASIC keeps track of subroutine
return addresses on a stack of its own when GOSUB and RETURN are used. FP and
SP indicate the places in the stack frame that the "frame pointer" and "stack
pointer" (respectively) point to; that is, as absolute numeric indices into
the array serving as the stack.
"""

import string

import mupas_stack
import mupas_static
import mupas_types

from typing import Collection, Iterable, Optional


class StringVariable(mupas_static.Value):
  """4050 BASIC's 26 string variables.

  Attributes:
    variable_name: A BASIC string variable name (e.g. "A$"), suitable for
        copying into BASIC code.
  """
  variable_name: str

  def __init__(self, variable_name: str):
    self.variable_name = variable_name

  def __str__(self) -> str:
    return self.variable_name


class NumericVariable(mupas_static.Value):
  """4050 BASIC's 286 numeric variables.

  Attributes:
    variable_name: A BASIC numeric variable name (e.g. "A"), suitable for
        copying into BASIC code.
  """
  variable_name: str

  def __init__(self, variable_name: str):
    self.variable_name = variable_name

  def __str__(self) -> str:
    return self.variable_name


class StackValue(mupas_stack.Value):
  """A scalar number stored on the (simulated) stack.

  Attributes:
    fp_offset: The value's offset from the frame pointer.
  """
  fp_offset: int

  def __init__(self, fp_offset: int):
    self.fp_offset = fp_offset

  def __str__(self) -> str:
    return f'[FP{self.fp_offset:+}]'


#### Static storage allocation, initialisation, and retirement. ####


class StaticAllocator(mupas_static.Allocator):
  """Static storage allocation details for Tektronix 4050 BASIC."""

  def __init__(self):
    self._numeric_variables = list(reversed(all_numeric_variables()))
    self._string_variables = list(reversed(all_string_variables()))

  class InitInfo(mupas_static.Allocator.InitInfo):
    """Information on how to initialise a 4050 BASIC static storage resource."""

    def __init__(self, resource: mupas_static.Value):
      self._resource = resource

    def code(self, typeinfo: mupas_types.Type) -> list[str]:
      """Returns code for initialising a storage resource for a data type."""

      # Code for dimensioning string variables.
      if isinstance(typeinfo, mupas_types.String):
        assert isinstance(self._resource, StringVariable)
        if not 1 <= typeinfo.length <= 65530:
          raise ValueError(
              f'String size of {typeinfo.length} is not in 1..65530')
        return [f'      DIM {self._resource.variable_name}({typeinfo.length})']

      # For dimensioning 1-D arrays.
      elif isinstance(typeinfo, mupas_types.Array1d):
        assert isinstance(self._resource, NumericVariable)
        i_typeinfo = typeinfo.index_typeinfo
        size = i_typeinfo.upper_bound - i_typeinfo.lower_bound + 1
        if not 1 <= size <= 8191:
          raise ValueError(f'Array size of {size} is not in 1..8191')
        return [f'      DIM {self._resource.variable_name}({size})']

      # For dimensioning 2-D arrays.
      elif isinstance(typeinfo, mupas_types.Array2d):
        assert isinstance(self._resource, NumericVariable)
        ri_typeinfo = typeinfo.row_index_typeinfo
        ci_typeinfo = typeinfo.col_index_typeinfo
        rows = ri_typeinfo.upper_bound - ri_typeinfo.lower_bound + 1
        cols = ci_typeinfo.upper_bound - ci_typeinfo.lower_bound + 1
        if not (1 <= rows <= 8191 and 1 <= cols <= 8191):
          raise ValueError(
              f'Array size of {rows},{cols} is not in 1..8191,1..8191')
        return [f'      DIM {self._resource.variable_name}({rows},{cols})']

      # Storage initialisation is not required for scalars.
      else:
        return []

  class DeleteInfo(mupas_static.Allocator.DeleteInfo):
    """Information on how to retire a 4050 BASIC static storage resource."""

    def __init__(self, resource: mupas_static.Value):
      self._resource = resource

    def code(self) -> list[str]:
      """Returns code for retiring a storage resource for a data type."""
      # This implementation is simple for now; it may expand later if needed.
      # An earlier implementation would only delete string and array variables,
      # but this implementation deletes all variables, since a different
      # subroutine may wish to DIM what was a scalar variable. This will yield
      # an error if the variable wasn't previously deleted.
      assert isinstance(self._resource, (StringVariable, NumericVariable))
      return [f'      DEL {self._resource.variable_name}']

  def allocate(self, mupas_type: mupas_types.Type) -> mupas_static.Value:
    # Strings go in string variables.
    if isinstance(mupas_type, mupas_types.String):
      try:
        return self._string_variables.pop()
      except IndexError:
        raise RuntimeError('Ran out of BASIC string variables') from None
    # Numeric values go in numeric variables.
    elif isinstance(mupas_type, (mupas_types.ScalarNumber,
                                 mupas_types.Array1d, mupas_types.Array2d)):
      try:
        return self._numeric_variables.pop()
      except IndexError:
        raise RuntimeError('Ran out of BASIC numeric/array variables') from None
    else:
      raise RuntimeError(
          f"No storage mechanism for {type(mupas_type)} (shouldn't happen)")

  def release(self, resource: mupas_static.Value):
    if isinstance(resource, NumericVariable):
      self._numeric_variables.append(resource)
    elif isinstance(resource, StringVariable):
      self._string_variables.append(resource)
    else:
      raise RuntimeError(f"Can't release a {type(resource)} (shouldn't happen)")

  @classmethod
  def init_info(cls, resource: mupas_static.Value) -> InitInfo:
    return cls.InitInfo(resource)

  @classmethod
  def delete_info(cls, resource: mupas_static.Value) -> DeleteInfo:
    return cls.DeleteInfo(resource)


def all_numeric_variables() -> list[NumericVariable]:
  """Assemble all numeric variables available in Tek 4050 BASIC."""
  variables = []
  for char in string.ascii_uppercase:
    variables.append(NumericVariable(char))
  for char in string.ascii_uppercase:
    for digit in string.digits:
      variables.append(NumericVariable(char + digit))
  return variables


def all_string_variables() -> list[StringVariable]:
  """Assemble all string variables available in Tek 4050 BASIC."""
  return [StringVariable(f'{c}$') for c in string.ascii_uppercase]


#### Stack storage allocation, initialisation, and retirement. ####


class Frame(mupas_stack.Frame):
  """Stack frame value bookkeeping details for Tektronix 4050 BASIC.

  Attributes:
    allocator: The frame allocator that allocated this frame.
    locals_position: At what position index in the "abstracted muPas stack
        frame" would storage for subroutine parameters and local variables
        begin? This attribute is less about where to find locals and more
        about delimiting the position where stack maintenance information
        (frame pointers etc.) ends. This is also where the frame pointer
        points.
    num_elements: Number of 4050 BASIC array elements actually used by the
        stack. This is different to the size of the abstracted muPas stack
        frame (as reported by len()), since some abstracted frame entries,
        like the return address, aren't needed for 4050 BASIC.
  """
  allocator: 'FrameAllocator'
  locals_position: int
  _stack_values: list[Optional[StackValue]]
  # One additional attribute: the original length of _stack_values, for
  # enforcing correct behaviour around growing stack frames.
  _original_len: int

  def __init__(self,
               allocator: 'FrameAllocator',
               stack_values: Iterable[Optional[StackValue]],
               locals_position: int):
    """Initialise a Frame.

    Args:
      allocator: The FrameAllocator allocating this frame.
      stack_values: An iterable whose n'th element is either stack storage
          information for the n'th item in the muPas abstracted stack frame
          or None if no storage is required for that item.
      locals_position: As described in the class docstring.
    """
    self.allocator = allocator
    self.locals_position = locals_position
    self._stack_values = list(stack_values)
    self._original_len = len(self._stack_values)

  class InitInfo(mupas_stack.Frame.InitInfo):
    """Information on how to grow the stack frame for various purposes."""

    def __init__(self, frame):
      self._sp = frame.allocator.stack_pointer_resource.variable_name

    def for_subroutine_parameter(self, stack_growth: int = 0) -> list[str]:
      """How to grow the stack frame for pushing a new subroutine parameter.

      Args:
        stack_growth: The stack frame may already have grown to have some
            leftover extra entries. We should recycle one of these.

      Returns:
        A list of 4050 BASIC instructions for growing the stack frame.
      """
      return [f'      {self._sp}={self._sp}{1-stack_growth:+}']

  class DeleteInfo(mupas_stack.Frame.DeleteInfo):
    """Information on how to shrink the stack frame for various purposes."""

    def __init__(self, frame):
      self._sp = frame.allocator.stack_pointer_resource.variable_name

    def for_n_places(self, places: int) -> list[str]:
      """Code for shrinking the stack frame by several places."""
      return [] if places == 0 else [f'      {self._sp}={self._sp}{-places:+}']

  def __getitem__(self, position: int) -> StackValue:
    value = self._stack_values[position]
    if isinstance(value, StackValue):
      return value
    else:
      raise RuntimeError(
          'Attempt to access a resource in a stack frame with no allocated '
          'storage. (Were we looking for a return address? Tek 4050 BASIC '
          "doesn't use those.)")

  def __len__(self) -> int:
    """Note that len() counts values not actually stored on the stack."""
    return len(self._stack_values)

  @property
  def num_elements(self):
    """How many entries in a stack array does this frame use?"""
    return sum(x is not None for x in self._stack_values)

  def allocate_for_function_call(self) -> int:
    """Grow the stack frame by three entries.

    Add bookkeeping information to this Frame for three more stack entries:
    space for a function return value, the old frame pointer, and the enclosing
    scope pointer. We don't need to add a None entry for the return address
    this time, since these stack entries are "internal" to the compiler's own
    mechanism for subroutine calls --- no other part of the compiler
    (in particular, no part external to the 4050 BASIC implementation) needs
    to know about this.

    Returns:
      The muPas abstract stack frame index for the first of the new entries,
      which you should use to retrieve the stack value you pass to `init_info`
      and `delete_info` methods.
    """
    return self._allocate_n(3)

  def allocate_for_procedure_call(self) -> int:
    """Grow the stack frame by two entries.

    As with `allocate_for_function_call`, but without allocating space for the
    return value.

    Returns:
      As with `allocate_for_function_call`.
    """
    return self._allocate_n(2)

  def allocate_for_subroutine_parameter(self) -> int:
    """Grow the stack frame by one entry.

    As with the other `allocate_for*` methods, but for single entries. Useful
    for advancing the stack when constructing parameters for procedure calls.

    Returns:
      As with `allocate_for_function_call`.
    """
    return self._allocate_n(1)

  def _allocate_n(self, num_entries) -> int:
    """Implementation backing the `allocate_for*` methods."""
    result = len(self._stack_values)
    top_of_stack = self._stack_values[-1]
    assert top_of_stack is not None  # Return addr shouldn't be here.
    self._stack_values.extend(
      StackValue(top_of_stack.fp_offset + 1 + i) for i in range(num_entries))
    return result

  def release_for_procedure_call(self, num_parameters: int):
    """Undo allocations `for_procedure_call` and `subroutine_parameter`s"""
    self.release_n(2 + num_parameters)

  def release_for_function_call(self, num_parameters: int):
    """Undo allocations `for_function_call` and `subroutine_parameter`s"""
    self.release_n(3 + num_parameters)

  def release_for_function_call_but_keep_result(self, num_parameters: int):
    """Undo most allocations `for_function_call` and `subroutine_parameter`s"""
    self.release_n(2 + num_parameters)

  def release_n(self, num_entries):
    """Implementation backing the release_for* methods."""
    if len(self._stack_values) - num_entries < self._original_len:
      raise RuntimeError(
          "Can't release stack frame entries that weren't allocated by the "
          'allocate_one method.')
    if num_entries > 0:
      self._stack_values[-num_entries:] = []

  def init_info(self, resource: mupas_stack.Value) -> InitInfo:
    del resource  # Unused in the current implementation.
    return self.InitInfo(self)

  def delete_info(self, resource: mupas_stack.Value) -> DeleteInfo:
    del resource  # Unused in the current implementation.
    return self.DeleteInfo(self)


class FrameAllocator(mupas_stack.FrameAllocator):
  """Stack frame storage allocation details for Tektronix 4050 BASIC."""
  stack_resource: NumericVariable
  stack_pointer_resource: NumericVariable
  frame_pointer_resource: NumericVariable

  def __init__(self,
               stack_resource: NumericVariable,
               stack_pointer_resource: NumericVariable,
               frame_pointer_resource: NumericVariable):
    self.stack_resource = stack_resource
    self.stack_pointer_resource = stack_pointer_resource
    self.frame_pointer_resource = frame_pointer_resource

  class InitInfo(mupas_stack.FrameAllocator.InitInfo):
    """Information on how to allocate a stack frame."""

    def __init__(self, frame: Frame):
      """Initialise an InitInfo.

      Args:
        frame: Stack frame bookkeeping details for the subroutine to be called.
      """
      self._frame = frame
      # These two values are for frame pointer offsets to the stack maintenance
      # information values of the frame under construction. These offsets are
      # relative to the **calling context's** frame pointer and will be set
      # by the `mark_stack_maintenance_offsets` method.
      self._old_frame_pointer_fp_offset: Optional[int] = None
      self._enclosing_context_fp_offset: Optional[int] = None

    def advance_stack_pointer_to_parameters(self) -> list[str]:
      """Returns code to prepare the stack pointer to start pushing parameters.

      Returns:
        Code that advances the stack pointer to the location of the first
        parameter for a subroutine call. This location is just beyond the
        return value (if a function) and stack maintenance values for the call,
        and is also marked by the locals_position attribute of a Frame.
      """
      # Shorthand.
      sp = self._frame.allocator.stack_pointer_resource.variable_name
      return [f'      {sp}={sp}+{self._frame.locals_position}']

    def mark_stack_maintenance_offsets(self, calling_frame: Frame):
      """Advise this InitInfo where to find critical stack values.

      Warning: See NOTE in args!

      Args:
        calling_frame: Stack frame bookkeeping details for the context that's
            calling the subroutine whose stack frame we're building. NOTE:
            The bookkeeping MUST have been just expanded to include slots for
            the return value (if this is a function) and the two stack
            maintenance values associated with all subroutine calls --- but NO
            further expansion (e.g. for function calls) should have been
            carried out yet.
      """
      self._old_frame_pointer_fp_offset = calling_frame[-2].fp_offset
      self._enclosing_context_fp_offset = calling_frame[-1].fp_offset

    def setup_frame_pointer(self, common_scope_hops: int) -> list[str]:
      """Returns code for finalising a stack frame for a subroutine call.

      Code returned by this method should be executed immediately after the
      last parameter has been pushed onto the stack. After that, the stack
      pointer should advance to allocate any more stack entries required by
      the subroutine, and then the program should execute the GOSUB jump to
      the subroutine itself.

      Args:
        common_scope_hops: Consider (a) the context calling the subroutine and
            (b) the subroutine being called. Both will have a line of common
            ancestor scopes. This number is the number of "generations" of
            beween the calling context and the first ancestor scope it shares
            with the called subroutine.

            Examples: (1) For a subroutine defined at "top level" calling
            another subroutine defined at "top level", this number is 1. (2) For
            a subroutine calling a subroutine defined within its own scope, this
            number is 0. (3) For a subroutine defined within the scope of a
            subroutine defined at "top level" calling a different subroutine
            defined at "top level", this number is 2.
      """
      assert (self._old_frame_pointer_fp_offset is not None and
              self._enclosing_context_fp_offset is not None), (
          'setup_frame_pointer() was called before stack maintenance offsets '
          'were marked with mark_stack_maintenance_offsets()')

      # Shorthand.
      stack = self._frame.allocator.stack_resource.variable_name
      fp = self._frame.allocator.frame_pointer_resource.variable_name

      # Assuming the stack pointer now points just beyond the last parameter
      # for the new function (which is where we need it to be), here are steps
      # for finalising the stack frame:
      maybe_offset = lambda x: f'{x:+}' if x else ''

      # 1. Save the frame pointer for the context calling the subroutine.
      fp_offset = maybe_offset(self._old_frame_pointer_fp_offset)
      old_frame_pointer =(f'      {stack}[{fp}{fp_offset}]={fp}')

      # 2. Save a pointer to the stack frame of the context that statically
      #    encloses the subroutine we're calling.
      fp_offset = maybe_offset(self._enclosing_context_fp_offset)
      enclosing_scope = f'{fp}'
      for _ in range(common_scope_hops):
        enclosing_scope = f'{stack}[{enclosing_scope}-1]'
      enclosing_scope = f'      {stack}[{fp}{fp_offset}]={enclosing_scope}'

      # 3. Compute the frame pointer for the called subroutine.
      fp_offset = maybe_offset(self._enclosing_context_fp_offset + 1)
      frame_pointer = f'      {fp}={fp}{fp_offset}'

      # 4. Allocate space for the called subroutine's local stack variables.

      return [old_frame_pointer, enclosing_scope, frame_pointer]

  class DeleteInfo(mupas_stack.FrameAllocator.DeleteInfo):
    """Information on how to retire a stack frame."""

    def __init__(self, frame: Frame):
      """Initialise an InitInfo.

      Args:
        frame: Stack frame bookkeeping details for the subroutine to be called.
      """
      self._frame = frame

    def code(self, keep_result: bool) -> list[str]:
      """Returns code for retiring a subroutine call's stack frame.

      Args:
        keep_result: If False, updates the stack pointer so it removes the
            entire stack frame from the subroutine call. If True, it leaves
            the first element of the stack frame on the stack, which for
            function calls is the return value. This option doesn't make much
            sense for procedure calls and should be avoided there.
      """
      # Shorthand.
      stack = self._frame.allocator.stack_resource.variable_name
      sp = self._frame.allocator.stack_pointer_resource.variable_name
      fp = self._frame.allocator.frame_pointer_resource.variable_name

      # Steps for freeing a stack frame:
      # 1. Recover the frame pointer for the calling context.
      # 2. Rewind the stack pointer to the end of the old stack frame, or
      return [f'      {fp}={stack}[{fp}-2]',
              f'      {sp}={sp}-{self._frame.num_elements-keep_result}']

  def allocate(self, mupas_typeinfos: Collection[mupas_types.Type]) -> Frame:
    # We'll be assigning stack locations to the items in mupas_types, which
    # lays out the types of values found in an abstracted muPas stack frame,
    # in order. There is not much complexity to this, since all values we
    # want to store on the stack are the same size regardless of type.
    storage: list[Optional[StackValue]] = []
    # That said, we don't waste any stack space on the return address, since
    # 4050 BASIC does its own bookkeeping for GOSUB/RETURN. Confusingly, we're
    # also using this variable that identifies the "locals position" (see the
    # Frame docstring) as a flag for whether we've also seen the muPas
    # abstracted stack frame's position for the return address. The "locals
    # position" is always 3 plus the return address position.
    locals_position = None
    frame_position = 0  # Counts up actual stack storage locations.
    for position, typeinfo in enumerate(mupas_typeinfos):
      if (isinstance(typeinfo, mupas_types.CodePointer)  # Is this the muPas
          and locals_position is None):  # abstract stack frame return address?
        storage.append(None)  # If so, no stack storage is allocated for it.
        locals_position = 2 + position  # Past old FP and enclosing scope ptr.
      else:
        # Allocate stack storage, but save a temporary offset relative to the
        # start of the stack frame, not the frame pointer/locals position,
        # which we may not know yet. We'll fix this later.
        storage.append(StackValue(frame_position))
        frame_position += 1

    # Here's where we change the stack frame offsets we've stored to offsets
    # relative to the frame pointer.
    assert locals_position is not None  # We have to have seen the ret. addr.
    for item in storage:
      if isinstance(item, StackValue):
        item.fp_offset -= locals_position

    # Return a Frame that contains this allocated storage.
    return Frame(self, storage, locals_position)

  def release(self, resource: mupas_stack.Frame):
    # There is nothing to do to release the storage used by the stack frame:
    # that portion of the array is "owned" by the stack.
    pass

  @classmethod
  def init_info(cls, resource: mupas_stack.Frame) -> InitInfo:
    assert isinstance(resource, Frame)
    return cls.InitInfo(resource)

  @classmethod
  def delete_info(cls, resource: mupas_stack.Frame) -> DeleteInfo:
    assert isinstance(resource, Frame)
    return cls.DeleteInfo(resource)


class StackAllocator(mupas_stack.StackAllocator):
  """Stack storage allocation details for Tektronix 4050 BASIC."""

  def __init__(self, stack_size: int, static_allocator: StaticAllocator):
    """Initialise a StackAllocator.

    Args:
      stack_size: Size of the stacks that this StackAllocator allocates.
      static_allocator: Stacks, stack pointers, and frame pointers, are kept
          in BASIC variables. These resources are allocated by this
          StackAllocator.
    """
    self._static_allocator = static_allocator
    self._stack_type = mupas_types.Array1d(
        index_typeinfo=mupas_types.IntegerSubrange(1, stack_size),
        value_typeinfo=mupas_types.Integer())
    self._stack_pointer_type = mupas_types.Integer()  # Same type for frame ptr.

  class InitInfo(mupas_stack.StackAllocator.InitInfo):
    """Information on how to initialise a 4050 BASIC stack.

    This class is just a container for a list of strings, as the init_info
    method will already have all the information it needs to say how to
    initialise a stack. (That information is: the 4050 BASIC resources used by
    the stack, plus the muPas types of data stored in those resources.)

    Attributes:
      code: Lines of code for initialising the stack.
    """
    code: list[str]
    def __init__(self, code: Iterable[str]):
      self.code = list(code)

  class DeleteInfo(mupas_stack.StackAllocator.DeleteInfo):
    """Information on how to retire a 4050 BASIC stack.

    This class is just a container for a list of strings, as the init_info
    method will already have all the information it needs to say how to retire
    a stack.

    Attributes:
      code: Lines of code for retiring the stack.
    """
    code: list[str]
    def __init__(self, code: Iterable[str]):
      self.code = list(code)

  def allocate(self) -> FrameAllocator:
    stack_resource = self._static_allocator.allocate(self._stack_type)
    stack_pointer_resource = self._static_allocator.allocate(
        self._stack_pointer_type)
    frame_pointer_resource = self._static_allocator.allocate(
        self._stack_pointer_type)
    assert isinstance(stack_resource, NumericVariable)
    assert isinstance(stack_pointer_resource, NumericVariable)
    assert isinstance(frame_pointer_resource, NumericVariable)
    return FrameAllocator(
        stack_resource, stack_pointer_resource, frame_pointer_resource)

  def release(self, resource: mupas_stack.FrameAllocator):
    """Indicate that resources for a stack are no longer needed.

    Args:
      resource: The FrameAllocator associated to the stack whose resources we
          are releasing. NOTE: Once passed to this method, the FrameAllocator
          should not be used any longer!
    """
    assert isinstance(resource, FrameAllocator)
    self._static_allocator.release(resource.stack_resource)
    self._static_allocator.release(resource.stack_pointer_resource)
    self._static_allocator.release(resource.frame_pointer_resource)

  def init_info(self, resource: mupas_stack.FrameAllocator) -> InitInfo:
    """Retrieve information about initialising a stack."""
    assert isinstance(resource, FrameAllocator)
    init_stack = self._static_allocator.init_info(
        resource.stack_resource).code(self._stack_type)
    init_stack_pointer = self._static_allocator.init_info(
        resource.stack_pointer_resource).code(self._stack_pointer_type)
    init_frame_pointer = self._static_allocator.init_info(
        resource.frame_pointer_resource).code(self._stack_pointer_type)
    # 4050 BASIC array indexing is 1-based, alas!
    return self.InitInfo(
        init_stack + init_stack_pointer + init_frame_pointer + [
            f'      {resource.stack_pointer_resource.variable_name}=1',
            f'      {resource.frame_pointer_resource.variable_name}=1'])

  def delete_info(self, resource: mupas_stack.FrameAllocator) -> DeleteInfo:
    """Retrieve information about how to retire a stack."""
    assert isinstance(resource, FrameAllocator)
    delete_stack = self._static_allocator.delete_info(
        resource.stack_resource).code()
    delete_stack_pointer = self._static_allocator.delete_info(
        resource.stack_pointer_resource).code()
    delete_frame_pointer = self._static_allocator.delete_info(
        resource.frame_pointer_resource).code()
    return self.DeleteInfo(
        delete_stack + delete_stack_pointer + delete_frame_pointer)
