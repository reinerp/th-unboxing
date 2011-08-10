{-# LANGUAGE TemplateHaskell #-}

-- | Instance of 'Lift' for 'SimpleCon'
module Data.Unboxed.LiftCon(SimpleCon(..), SimpleData(..), simpleCon, unSimpleCon, toName, fromName, Lift) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

-- names
type SimpleName = String

toName :: SimpleName -> Name
toName = mkName

fromName :: Name -> SimpleName
fromName = nameBase

-- types
data SimpleType
 = VarTS SimpleName
 | ConTS SimpleName
 | AppTS SimpleType SimpleType
 | TupleTS Int
 | ArrowTS
 | ListTS
 | SigTS SimpleType Kind
 | ForallTS [SimpleTyVarBndr] SimpleCxt SimpleType

toType :: SimpleType -> Type
toType (VarTS nm) = VarT (toName nm)
toType (ConTS nm) = ConT (toName nm)
toType (AppTS t1 t2) = AppT (toType t1) (toType t2)
toType (TupleTS n) = TupleT n
toType ArrowTS = ArrowT
toType ListTS = ListT
toType (SigTS ty k) = SigT (toType ty) k
toType (ForallTS bndrs cxt t) = ForallT (map toBndr bndrs) (map toPred cxt) (toType t)

fromType :: Type -> SimpleType
fromType (VarT nm) = VarTS (fromName nm)
fromType (ConT nm) = ConTS (fromName nm)
fromType (AppT t1 t2) = AppTS (fromType t1) (fromType t2)
fromType (TupleT n) = TupleTS n
fromType ArrowT = ArrowTS
fromType ListT = ListTS
fromType (SigT ty k) = SigTS (fromType ty) k
fromType (ForallT bndrs cxt t) = ForallTS (map fromBndr bndrs) (map fromPred cxt) (fromType t)

data SimpleTyVarBndr
  = PlainTVS SimpleName
  | KindedTVS SimpleName Kind

toBndr :: SimpleTyVarBndr -> TyVarBndr
toBndr (PlainTVS nm) = PlainTV (toName nm)
toBndr (KindedTVS nm k) = KindedTV (toName nm) k

fromBndr :: TyVarBndr -> SimpleTyVarBndr
fromBndr (PlainTV nm) = PlainTVS (fromName nm)
fromBndr (KindedTV nm k) = KindedTVS (fromName nm) k

type SimpleCxt = [SimplePred]

data SimplePred
  = ClassPS SimpleName [SimpleType]
  | EqualPS SimpleType SimpleType
  
toPred :: SimplePred -> Pred
toPred (ClassPS nm tys) = ClassP (toName nm) (map toType tys)
toPred (EqualPS t1 t2) = EqualP (toType t1) (toType t2)

fromPred :: Pred -> SimplePred
fromPred (ClassP nm tys) = ClassPS (fromName nm) (map fromType tys)
fromPred (EqualP t1 t2) = EqualPS (fromType t1) (fromType t2)

type SimpleStrictType = (Strict, SimpleType)

toStrictType :: SimpleStrictType -> StrictType
toStrictType (str, ty) = (str, toType ty)

fromStrictType :: StrictType -> SimpleStrictType
fromStrictType (str, ty) = (str, fromType ty)

-- constructors
data SimpleCon' = SimpleCon' [SimpleTyVarBndr] SimpleCxt SimpleName [SimpleStrictType]

toSimpleCon :: SimpleCon' -> SimpleCon
toSimpleCon (SimpleCon' bndrs cxt nm tys) = SimpleCon (map toBndr bndrs) (map toPred cxt) (toName nm) (map toStrictType tys)

fromSimpleCon :: SimpleCon -> SimpleCon'
fromSimpleCon (SimpleCon bndrs cxt nm tys) = SimpleCon' (map fromBndr bndrs) (map fromPred cxt) (fromName nm) (map fromStrictType tys)

instance Lift SimpleCon where
  lift con = [| toSimpleCon $(lift (fromSimpleCon con)) |]
instance Lift SimpleData where
  lift (SimpleData nm bndrs cons) = [| SimpleData (toName $(lift $ fromName nm)) (map toBndr $(lift $ map fromBndr bndrs)) cons |]

-- old interface
data SimpleData = SimpleData Name [TyVarBndr] [SimpleCon]

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

$(deriveLiftMany [''SimpleType, ''SimpleTyVarBndr, ''SimplePred, ''Kind, ''Strict, ''SimpleCon'])

-- $(deriveLiftMany [''SimpleCon, ''Strict, ''TyVarBndr, ''Pred, ''Type, ''Kind])
