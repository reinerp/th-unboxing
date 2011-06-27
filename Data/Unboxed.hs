{-# LANGUAGE TypeFamilies #-}

module Data.Unboxed where

data family Polymorphic a

class Unpacked a where
  {-# INLINE mk #-}
  mk :: Polymorphic a -> a
  {-# INLINE unMk #-}
  unMk :: a -> Polymorphic a
