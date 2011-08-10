{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances, CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

#ifdef VACUUM
import System.Vacuum.Cairo
#endif
import UnboxedPair

$(definePair [t| Pair Int Int |])
-- unfortunately doesn't unpack, due to <http://hackage.haskell.org/trac/ghc/ticket/3990>
-- $(definePair [t| Pair (Pair Int Int) Int |])

simplePair :: Pair Int Int
simplePair = pair 0 1

--nestedPair :: Pair (Pair Int Int) Int
--nestedPair = pair (pair 0 1) 2

#ifdef VACUUM
main = 
  do viewFile "unboxpair.svg" (foo, bar, bam, (0,1))
     putStrLn "Data-structure diagram written to unboxpair.svg"
#else
main = putStrLn "Test compiled (and ran) successfully!"
#endif

