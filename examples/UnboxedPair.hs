{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts, ViewPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module UnboxedPair(
  Pair, -- the 'Pair' family
  Polymorphic(Pair), -- the 'Pair' constructor
  definePair,
  pair,
  unPair
  ) where

import Data.Unboxed.Derive

$(declareType "definePair" [d| data Pair a b = Pair !a !b |])

pair :: Unpacked (Pair a b) => a -> b -> Pair a b
pair a b = mk (Pair a b)

unPair :: Unpacked (Pair a b) => Pair a b -> (a, b)
unPair (unMk -> Pair a b) = (a, b)