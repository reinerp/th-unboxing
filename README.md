Unboxed containers
==================

The aim of this package is to reduce the boilerplate required declaring "unboxed containers" via data families.

We would like to write

  data Foo a = Foo {-# UNPACK #-} !a

so that a 'Foo Int' takes just as much space as 'Int' (currently it takes 2 words more, and involves an extra indirection). A standard approach to this is to use data families:

  data family Foo a
  class HasFoo a where
      data Foo a
      mkFoo :: a -> Foo a
      unmkFoo :: Foo -> a
  ...
  instance HasFoo Int where
      data Foo Int = FooInt {-# UNPACK #-} !Int
      mkFoo = FooInt
      unmkFoo (Foo a) = a

A number of people have pointed out that this approach doesn't scale: unlike for classes such as 'Binary' or 'Data.Vector.Unboxed.Unbox', we can't write an instance

  instance (HasFoo a, HasFoo b) => HasFoo (a, b)

which unpacks both @a@ and @b@ (try writing one!).

This library *doesn't* solve this problem. The problem it solves is a simpler one: it removes most of the boilerplate involved in comparing the two code snippets above. With this library, you can write

  $(declareType "deriveFoo" [d| data Foo a = Foo !a |])
  ...
  $(deriveFoo [t| Foo Int |])

See examples/Main.hs for a complete example.
