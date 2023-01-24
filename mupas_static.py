"""Abstractions for muPas static storage.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.

During the analysis phase (and prior to code generation), the compiler may
allocate some muPas constants, variables, and subroutine parameters to various
static resources. The nature of these static resources are target-specific; the
abstraction that the compiler requires pre-generaton is fairly minimal, and
that's what's laid out by the abstract superclass defined in this module.
Subclasses will be defined by target-specific modules.

Note: In documentation for this module, "Claim" and "Indicate" describe a
discussion happening inside the compiler, not inside the compiled program or
the target platform/environment.
"""

import abc

import mupas_resources
import mupas_types


class Value(mupas_resources.Value):
  """Superclass for a place where a static value is stored."""
  def __str__(self) -> str:
    return '<static item>'  # A fallback; subclasses should supply better


class Allocator(abc.ABC):
  """Superclass for bookkeeping around stored static global values.

  The init_info and delete_info methods in most subclasses of Allocator may
  often be static methods in spirit (and perhaps static or class methods in
  implementation), but they don't have to be.
  """

  class InitInfo:
    """Superclass for information about initialising a static value resource.

    The contents of this class will vary considerably depending on the target
    platform, but it should contain enough information for the code generation
    machinery to generate code that prepares that value resource for use by
    other compiled code.
    """

  class DeleteInfo:
    """Superclass for information about retiring a static value resource.

    The contents of this class will vary considerably depending on the target
    platform, but it should contain enough information for the code generation
    machinery to generate code that deallocates, de-initialises, frees, or
    otherwise retires that resource.
    """

  @abc.abstractmethod
  def allocate(self, mupas_type: mupas_types.Type) -> Value:
    """Claim an unused resource for a static value of a certain type."""

  @abc.abstractmethod
  def release(self, resource: Value):
    """Indicate that a static value resource is no longer used."""

  @abc.abstractmethod
  def init_info(self, resource: Value) -> InitInfo:
    """Retrieve information about initialising a static value resource."""

  @abc.abstractmethod
  def delete_info(self, resource: Value) -> DeleteInfo:
    """Retrieve information about how to retire a static value resource."""
