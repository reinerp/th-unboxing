{-# LANGUAGE TemplateHaskell #-}

-- | Instance of 'Lift' for 'SimpleCon'
module Data.Unboxed.LiftCon(SimpleCon(..), simpleCon, unSimpleCon, toName, fromName, Lift) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

-- unqualified...
type SimpleName = String

toName :: SimpleName -> Name
toName = mkName

fromName :: Name -> SimpleName
fromName = nameBase

data SimpleCon = SimpleCon [TyVarBndr] Cxt SimpleName [StrictType]

simpleCon :: Con -> SimpleCon
simpleCon (NormalC nm stys) = SimpleCon [] [] (fromName nm) stys
simpleCon (RecC nm vstys) = SimpleCon [] [] (fromName nm) (map (\(_,s,t) -> (s,t)) vstys)
simpleCon (InfixC st1 nm st2) = SimpleCon [] [] (fromName nm) [st1, st2]
simpleCon (ForallC bndrs cxt con) = case simpleCon con of
  SimpleCon [] [] nm ts -> SimpleCon bndrs cxt nm ts

unSimpleCon :: SimpleCon -> Con
unSimpleCon (SimpleCon [] [] nm ts) = NormalC (toName nm) ts
unSimpleCon (SimpleCon bndrs cxt nm ts) = ForallC bndrs cxt (NormalC (toName nm) ts)

$(deriveLiftMany [''SimpleCon, ''Strict, ''TyVarBndr, ''Pred, ''Type, ''Kind])