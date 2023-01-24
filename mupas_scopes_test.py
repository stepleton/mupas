"""Tests for the mupas_scopes module.

Forfeited into the public domain with NO WARRANTY. Read LICENSE for details.
"""

import unittest

import mupas_scopes


class MupasScopesTest(unittest.TestCase):
  """Test harness for testing the mupas_scopes module."""

  def test_single_scope(self):
    """Check insertion/retrival/removal/etc. for just one scope."""

    scope : mupas_scopes.Scope[int] = mupas_scopes.Scope('Scope')
    self.assertFalse('item' in scope)

    # Insertion and retrieval.
    scope['item'] = 5
    self.assertTrue('item' in scope)
    self.assertEqual(scope['item'], 5)
    self.assertEqual(scope.itempath('item'), '/Scope/item')
    with self.assertRaisesRegex(KeyError, 'Symbol item is already bound'):
      scope['item'] = 10

    # Substitution and temporary bindings.
    with scope.substitute(item=10, temp=20):
      self.assertTrue('temp' in scope)
      self.assertEqual(scope['item'], 10)
      self.assertEqual(scope['temp'], 20)
    self.assertFalse('temp' in scope)
    self.assertEqual(scope['item'], 5)

    # Removal and errors with missing bindings.
    del scope['item']
    self.assertFalse('item' in scope)
    with self.assertRaisesRegex(KeyError, 'Symbol item is unbound'):
      _ = scope['item']
    with self.assertRaisesRegex(KeyError, 'Symbol item is unbound'):
      _ = scope.itempath('item')
    with self.assertRaisesRegex(KeyError, 'Symbol item is unbound'):
      del scope['item']

  def test_nested_scope(self):
    """Check scope operations with nested scopes."""

    # Constructing two scopes with a parent/child relationship.
    parent : mupas_scopes.Scope[int] = mupas_scopes.Scope('Parent')
    parent['p_item'] = 5
    child : mupas_scopes.Scope[int] = mupas_scopes.Scope('Child', parent=parent)
    child['c_item'] = 10
    self.assertEqual(parent, child.parent)

    # Queries.
    self.assertTrue('p_item' in child)
    self.assertTrue('c_item' in child)
    self.assertFalse('c_item' in parent)
    self.assertEqual(child.itempath('p_item'), '/Parent/p_item')
    self.assertEqual(child.itempath('c_item'), '/Parent/Child/c_item')
    self.assertEqual(child.itemhops('p_item'), 1)
    self.assertEqual(child.itemhops('c_item'), 0)

    # Deletions must occur within the defining scope.
    with self.assertRaisesRegex(KeyError, 'Symbol p_item is in a scope enclos'):
      del child['p_item']

    # Retrieval and demonstration of name masking.
    parent['c_item'] = 15
    self.assertEqual(5, child['p_item'])
    self.assertEqual(10, child['c_item'])
    self.assertEqual(15, parent['c_item'])
    self.assertEqual(parent.itempath('c_item'), '/Parent/c_item')
    self.assertEqual(child.itempath('c_item'), '/Parent/Child/c_item')
    self.assertEqual(parent.itemhops('p_item'), 0)
    self.assertEqual(child.itemhops('c_item'), 0)

  def test_biscope(self):
    """Check parent/child bookkeeping within BiScopes."""

    parent : mupas_scopes.BiScope[int] = mupas_scopes.BiScope('Parent')
    c_1 : mupas_scopes.BiScope[int] = mupas_scopes.BiScope('C1', parent=parent)
    c_2 : mupas_scopes.BiScope[int] = mupas_scopes.BiScope('C2', parent=parent)

    self.assertDictEqual(parent.children, {'C1': c_1, 'C2': c_2})

    with self.assertRaisesRegex(KeyError, '/Parent already has a child scope'):
      _ = mupas_scopes.BiScope('C1', parent=parent)

    self.assertEqual(parent.get_scope('/Parent'), parent)
    self.assertEqual(parent.get_scope('/Parent/C1'), c_1)
    self.assertEqual(parent.get_scope('/Parent/C2'), c_2)
    with self.assertRaisesRegex(KeyError, 'No scope /Uncle within Parent'):
      parent.get_scope('/Uncle')
    with self.assertRaisesRegex(KeyError, 'No scope /Parent/C4 within Parent'):
      parent.get_scope('/Parent/C4')
    parent.del_child('C1')
    with self.assertRaisesRegex(KeyError, 'No scope /Parent/C1 within Parent'):
      parent.get_scope('/Parent/C1')
