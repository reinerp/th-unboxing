{-# LANGUAGE TypeFamilies #-}

module Data.Unboxed where

data family Polymorphic a


class Unpacked a where
--  {-# INLINE mk #-} -- new GHC doesn't like this
  mk :: Polymorphic a -> a
--  {-# INLINE unMk #-}
  unMk :: a -> Polymorphic a
