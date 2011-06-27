{-# LANGUAGE TemplateHaskell #-}

-- | Instance of 'Lift' for 'SimpleCon'
module Data.Unboxed.LiftCon(SimpleCon(..), simpleCon, unSimpleCon, Lift) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

data SimpleCon = SimpleCon [TyVarBndr] Cxt Name [StrictType]

simpleCon :: Con -> SimpleCon
simpleCon (NormalC nm stys) = SimpleCon [] [] nm stys
simpleCon (RecC nm vstys) = SimpleCon [] [] nm (map (\(_,s,t) -> (s,t)) vstys)
simpleCon (InfixC st1 nm st2) = SimpleCon [] [] nm [st1, st2]
simpleCon (ForallC bndrs cxt con) = case simpleCon con of
  SimpleCon [] [] nm ts -> SimpleCon bndrs cxt nm ts

unSimpleCon :: SimpleCon -> Con
unSimpleCon (SimpleCon [] [] nm ts) = NormalC nm ts
unSimpleCon (SimpleCon bndrs cxt nm ts) = ForallC bndrs cxt (NormalC nm ts)

$(deriveLiftMany [''SimpleCon, ''Strict, ''TyVarBndr, ''Pred, ''Type, ''Kind])