"""Abstractions for muPas stacks, stack frames, and values stored thereon.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

During the analysis phase (and prior to code generation), the compiler may
allocate some muPas variables and subroutine parameters to locations within
imaginary stack frames that live on an imaginary stack. These constructs are a
fiction within the compiler: a target architecture may use a stack and stack
frames that are laid out differently to the ones here, or it may not use a
stack at all!

Echoing the hardships of dealing with a real stack, the locations within
muPas abstract stack frames are identified only by number (cf. array indices).
Certain stack frame positions have special meanings: for details and a diagram,
refer to the docstring for the `Frame` class.

This module only defines abstract base classes: subclasses of these classes
are target-specific. These subclasses will include details for generating
code for creating stack frames, accessing stack frame values, and so on.

Note: In documentation for this module, "Claim" and "Indicate" describe a
discussion happening inside the compiler, not inside the compiled program or
the target platform/environment.
"""

import abc

import mupas_resources
import mupas_types

from typing import Collection


class Value(mupas_resources.Value):
  """Superclass for a place on the stack where a value is stored."""


class Frame(abc.ABC, mupas_resources.Frame):
  """Superclass for bookkeeping around data stored in stack frames.

  There are no "allocate" or "release" methods: this model assumes that space
  for stack frames and their value contents is allocated and released all at
  once (see Allocator).

  Note that stack frame indices (for __getitem__) and length (for __len__) are
  in reference to the "abstracted muPas stack frame", which takes this form:

     [ Return value (if a function)  ]   <Positive, incrementing indices>
     [ Return address                ]
     [ Old frame pointer             ]     |  Direction of stack growth
     [ Pointer to enclosing scope    ]     |
     [ Subroutine scalar parameter 1 ]   \ | /
     [ Subroutine scalar parameter 2 ]    \|/
     [ ...                           ]     v
     [ Local scalar variable 1       ]
     [ Local scalar variable 2       ]
     [ ...                           ]

  The ways in which these elements are actually stored are target-dependent.
  Some targets might not use a stack at all!
  """

  class InitInfo:
    """Superclass for information about initialising a stack value resource.

    The contents of this class will vary considerably depending on the target
    platform, but it should contain enough information for the code generation
    machinery to generate code that prepares that value resource for use by
    other compiled code.
    """

  class DeleteInfo:
    """Superclass for information about retiring a stack value resource.

    The contents of this class will vary considerably depending on the target
    platform, but it should contain enough information for the code generation
    machinery to generate code that deallocates, de-initialises, frees, or
    otherwise retires that resource.
    """

  @abc.abstractmethod
  def __getitem__(self, slot: int) -> Value:
    """Retrieve information about a resource at a stack frame position."""

  @abc.abstractmethod
  def __len__(self) -> int:
    """Size of the "abstracted muPas stack frame" implemented by this Frame."""

  @abc.abstractmethod
  def init_info(self, resource: Value) -> InitInfo:
    """Retrieve information about how to initialise a stack value resource.

    Args:
      resource: A value resource associated with this stack frame.

    Returns: as described.
    """
    pass

  @abc.abstractmethod
  def delete_info(self, resource: Value) -> DeleteInfo:
    """Retrieve information about how to retire a stack value resource.

    Args:
      resource: A value resource associated with this stack frame.

    Returns: as described.
    """
    pass


class FrameAllocator(abc.ABC):
  """Superclass for bookeeping around whole stack frames."""

  class InitInfo:
    """Superclass for information about initialising a stack frame.

    The contents of this class will vary considerably depending on the target
    platform, but it should contain enough information for the code generation
    machinery to generate code that prepares that stack frame for use by other
    compiled code.
    """

  class DeleteInfo:
    """Superclass for information about retiring a stack frame.

    The contents of this class will vary considerably depending on the target
    platform, but it should contain enough information for the code generation
    machinery to generate code that deallocates, de-initialises, frees, or
    otherwise retires that stack frame.
    """

  @abc.abstractmethod
  def allocate(self, mupas_typeinfos: Collection[mupas_types.Type]) -> Frame:
    """Claim resources for a stack frame containing certain values."""
    pass

  @abc.abstractmethod
  def release(self, resource: Frame):
    """Indicate that resources for a stack frame are no longer needed."""
    pass

  @abc.abstractmethod
  def init_info(self, resource: Frame) -> InitInfo:
    """Retrieve information about initialising a stack frame."""
    pass

  @abc.abstractmethod
  def delete_info(self, resource: Frame) -> DeleteInfo:
    """Retrieve information about how to retire a stack frame."""
    pass


class StackAllocator(abc.ABC):
  """Superclass for bookeeping around whole stacks."""

  class InitInfo:
    """Superclass for information about initialising a stack.

    The contents of this class will vary considerably depending on the target
    platform, but it should contain enough information for the code generation
    machinery to generate code that prepares that stack for use by other
    compiled code.
    """

  class DeleteInfo:
    """Superclass for information about retiring a stack.

    The contents of this class will vary considerably depending on the target
    platform, but it should contain enough information for the code generation
    machinery to generate code that deallocates, de-initialises, frees, or
    otherwise retires that stack.
    """

  @abc.abstractmethod
  def allocate(self) -> FrameAllocator:
    """Claim resources for a stack."""
    pass

  @abc.abstractmethod
  def release(self, resource: FrameAllocator):
    """Indicate that resources for a stack are no longer needed."""
    pass

  @abc.abstractmethod
  def init_info(self, resource: FrameAllocator) -> InitInfo:
    """Retrieve information about initialising a stack."""
    pass

  @abc.abstractmethod
  def delete_info(self, resource: FrameAllocator) -> DeleteInfo:
    """Retrieve information about how to retire a stack."""
    pass
