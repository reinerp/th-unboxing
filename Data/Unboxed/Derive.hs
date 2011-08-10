{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{- |
The aim of this package is to reduce the boilerplate required declaring "unboxed containers" via data families.

We would like to write

@data Foo a = Foo \{\-\# UNPACK \#\-\} !a@

so that a 'Foo Int' takes just as much space as 'Int' (currently it takes 2 words more, and involves an extra indirection). A standard approach to this is to use data families:

@data family Foo a
class HasFoo a where
    data Foo a
    mkFoo :: a -> Foo a
    unmkFoo :: Foo -> a
...
instance HasFoo Int where
    data Foo Int = FooInt \{\-\# UNPACK \#\-\} !Int
    mkFoo = FooInt
    unmkFoo (Foo a) = a@

A number of people have pointed out that this approach doesn't scale: unlike for classes such as 'Binary' or 'Data.Vector.Unboxed.Unbox', we can't write an instance

>  instance (HasFoo a, HasFoo b) => HasFoo (a, b)

which unpacks both @a@ and @b@ (try writing one!).

This library *doesn't* solve this problem. The problem it solves is a simpler one: it removes most of the boilerplate involved in comparing the two code snippets above. With this library, you can write

@\$('declareType' \"deriveFoo\" [d| data Foo a = Foo !a |])
...
\$(deriveFoo [t| Foo Int |])@

A complete example follows:

@\{\-\# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts, ViewPatterns \#\-\}
\{\-\# OPTIONS_GHC -funbox-strict-fields \#\-\}

module UnboxedPair where
import Data.Unboxed.Derive

\$('declareType' \"definePair\" [d| data Pair a b = Pair !a !b |])

pair :: 'Unpacked' (Pair a b) => a -> b -> Pair a b
pair a b = 'mk' (Pair a b)

unPair :: Unpacked (Pair a b) => Pair a b -> (a, b)
unPair ('unMk' -> Pair a b) = (a, b)@

and a client of the UnboxedPair module:

@\{\-\# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances \#\-\}
\{\-\# OPTIONS_GHC -funbox-strict-fields \#\-\}
import UnboxedPair

\$(definePair [t| Pair Int Int |])@

The @Pair@ constructor is a constructor for the type @'Polymorphic' (Pair a b)@, and as such can be exported as

> module UnboxedPair('Polymorphic'(Pair)) where ...

Note also the LANGUAGE pragmas (which are necessary, other than ViewPatterns), and the -funbox-strict-fields flag. This flag is necessary to ensure unboxing, because Template Haskell doesn't support the {\-# UNPACK #-\} pragma.
-}
module Data.Unboxed.Derive(declareType, module Data.Unboxed) where

import Control.Applicative
import Control.Monad
import Data.Unboxed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Unboxed.LiftCon
import Data.Maybe
import Control.Arrow(second)

thAssert :: Show s => s -> String -> Bool -> Q ()
thAssert d s assertion = unless assertion $ report True ("Data.Unpacked.Derive: " ++ s ++ "\nIn datatype: " ++ show d)

mkExpApp :: Exp -> [Exp] -> Exp
mkExpApp e exps = foldl AppE e exps

mkTyApp :: Name -> [Type] -> Type
mkTyApp nm ts = foldl AppT (ConT nm) ts

unTyApp :: Type -> Maybe (Name,[Type])
unTyApp (ConT nm) = pure (nm,[])
unTyApp (AppT a b) = second (++ [b]) <$> unTyApp a
unTyApp _ = Nothing

declareType :: String -> Q [Dec] -> Q [Dec]
declareType s qds = qds >>= (\[ds] -> declareType' s ds)

-- creates the data family, the 'Polymorphic' instance, and the deriver by the given name
declareType' :: String -> Dec -> Q [Dec]
declareType' deriverNm d@(DataD cxt nm tyvars cons derivs) = do
  thAssert d "Data-type contexts not supported" $ null cxt
  thAssert d "deriving instances not YET supported" $ null derivs
  thAssert d "Type should be polymorphic" $ (not . null) tyvars

  -- define the family
  let fam = FamilyD DataFam nm tyvars Nothing
  
  let polyTy = mkTyApp nm (map (VarT . bndrName) tyvars)

  -- define the 'Polymorphic' instance
  let inst = DataInstD [] ''Polymorphic [polyTy] cons []
      
  -- define the deriver
  ctorsExp <- lift (SimpleData nm tyvars (map simpleCon cons))
  let deriver = FunD (mkName deriverNm) [Clause [] (NormalB (AppE (VarE 'declareTypeInstance) ctorsExp)) []]
{-  let dInst = InstanceD [] (AppT (ConT ''Derivable) polyTy) [
        FunD 'getDataD [Clause [WildP] (NormalB ctorsExp) []]
        ]
 -} 
  return [fam,inst,deriver]

substVar :: (Name -> Maybe Type) -> Type -> Type
substVar f = subst where
  -- here the substitution happens
  subst (VarT nm) = fromMaybe (VarT nm) (f nm)
  -- everywhere else, just recurse
  subst (ForallT bndrs cxt ty) = ForallT bndrs cxt (subst ty)
  subst (AppT t1 t2) = AppT (subst t1) (subst t2)
  subst (SigT t k) = SigT (subst t) k
  -- the rest have no type variables in them
  subst t = t

bndrName :: TyVarBndr -> Name
bndrName (PlainTV nm) = nm
bndrName (KindedTV nm _) = nm

inline :: String -> Dec
inline name = PragmaD (InlineP (mkName name) (InlineSpec True False Nothing))

declareTypeInstance :: SimpleData -> Q Type -> Q [Dec]
declareTypeInstance a qt = do
  t <- qt
  declareTypeInstance' a t

-- creates the 'data instance' and the 'instance Unpacked'
declareTypeInstance' :: SimpleData -> Type -> Q [Dec]
declareTypeInstance' (SimpleData _ (map bndrName -> tyvars) cons) t@(unTyApp -> Just (nm,tys)) = do
  thAssert t "Applied to wrong number of type variables" $ length tys == length tyvars

  let tyLookup = zip tyvars tys
  -- rename the constructors, substitute the ty vars
  cons' <- forM cons $ \(SimpleCon bndrs cxt nm stys) -> do
    -- nm' <- newName (nameBase nm) --
    let nm' = mkName (nameBase nm ++ "__unpacked")
    --let nm' = nameBase nm ++ "__unpacked" -- urgh
    return $ SimpleCon bndrs cxt nm' (map (second . substVar $ flip lookup tyLookup) stys)
  -- construct the datatype instance
  let dataInstD = DataInstD [] nm tys (map unSimpleCon cons') []

  -- now construct the 'instance Unpacked'
  let mkClause (SimpleCon _ _ nm1 (length -> l)) (SimpleCon _ _ nm2 _) = do
        vars <- mapM newName (replicate l "v")
        return $ Clause [ConP nm1 (map VarP vars)] (NormalB (mkExpApp (ConE nm2) (map VarE vars))) []
      mkClauses cons1 cons2 = sequence (zipWith mkClause cons1 cons2)
      
  mkCls <- mkClauses cons cons'
  unMkCls <- mkClauses cons' cons
  let classInstD = InstanceD [] (AppT (ConT ''Unpacked) t) [
        inline "mk",
        inline "unMk",
        FunD 'mk mkCls,
        FunD 'unMk unMkCls
        ]

  return [dataInstD, classInstD]
