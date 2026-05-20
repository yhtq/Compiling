module Ast where
import Data.Text (Text)
import qualified Data.Text as T
import Text (TShow(..), IsText (fromText))
import Data.Hashable (Hashable(..))

-- LC AST
data Term lit v = Var v
        | Lit lit
        | Lam v (Term lit v)
        | App (Term lit v) (Term lit v)
        | OLet v (Term lit v) (Term lit v)
        | RLet v (Term lit v) (Term lit v)
    deriving (Ord, Eq, Show, Foldable, Functor, Traversable)

-- 带变量的类型仅作为类型推导的中间状态使用
data Typ lit v = TVar v
        | TFun (Typ lit v) (Typ lit v)
        | TLit lit
    deriving (Ord, Eq, Show, Foldable, Functor, Traversable)

data TypedTerm typ lit v = VarT v typ
        | LitT lit typ
        | LamT v typ (TypedTerm typ lit v) typ
        | AppT (TypedTerm typ lit v) (TypedTerm typ lit v) typ
        | OLetT v typ (TypedTerm typ lit v) (TypedTerm typ lit v) typ
        | RLetT v typ (TypedTerm typ lit v) (TypedTerm typ lit v) typ
    deriving (Ord, Eq, Show, Foldable, Functor, Traversable)

instance (TShow lit, TShow v) => TShow (Term lit v) where
    tshow (Var v) = tshow v
    tshow (Lit l) = tshow l
    tshow (Lam v body) = "λ" <> tshow v <> ". " <> tshow body
    tshow (App f x) = "(" <> tshow f <> " " <> tshow x <> ")"
    tshow (OLet v b1 b2) = "let " <> tshow v <> " = " <> tshow b1 <> " in " <> tshow b2
    tshow (RLet v b1 b2) = "rec " <> tshow v <> " = " <> tshow b1 <> " in " <> tshow b2

instance (TShow lit, TShow v) => TShow (Typ lit v) where
    tshow (TVar v) = tshow v
    tshow (TFun a b) = "(" <> tshow a <> " -> " <> tshow b <> ")"
    tshow (TLit l) = tshow l

instance (TShow lit, TShow v, TShow typ) => TShow (TypedTerm typ lit v) where
    tshow (VarT v t) = tshow v <> " : " <> tshow t
    tshow (LitT l t) = tshow l <> " : " <> tshow t
    tshow (LamT v vt body _) = "\\" <> tshow v <> " -> " <> tshow vt <> ". " <> tshow body
    tshow (AppT f x _) = "(" <> tshow f <> " " <> tshow x <> ")"
    tshow (OLetT v vt b1 b2 _) = "let " <> tshow v <> " : " <> tshow vt <> " = " <> tshow b1 <> " in " <> tshow b2  
    tshow (RLetT v vt b1 b2 _) = "let rec " <> tshow v <> " : " <> tshow vt <> " = " <> tshow b1 <> " in " <> tshow b2

instance (Hashable lit, Hashable v) => Hashable (Term lit v) where
    hashWithSalt s (Var v) = s `hashWithSalt` (0 :: Int) `hashWithSalt` v
    hashWithSalt s (Lit l) = s `hashWithSalt` (1 :: Int) `hashWithSalt` l
    hashWithSalt s (Lam v body) = s `hashWithSalt` (2 :: Int) `hashWithSalt` v `hashWithSalt` body
    hashWithSalt s (App f x) = s `hashWithSalt` (3 :: Int) `hashWithSalt` f `hashWithSalt` x
    hashWithSalt s (OLet v b1 b2) = s `hashWithSalt` (4 :: Int) `hashWithSalt` v `hashWithSalt` b1 `hashWithSalt` b2
    hashWithSalt s (RLet v b1 b2) = s `hashWithSalt` (5 :: Int) `hashWithSalt` v `hashWithSalt` b1 `hashWithSalt` b2
instance (Hashable lit, Hashable v) => Hashable (Typ lit v) where
    hashWithSalt s (TVar v) = s `hashWithSalt` (0 :: Int) `hashWithSalt` v
    hashWithSalt s (TFun a b) = s `hashWithSalt` (1 :: Int) `hashWithSalt` a `hashWithSalt` b
    hashWithSalt s (TLit l) = s `hashWithSalt` (2 :: Int) `hashWithSalt` l

instance (Hashable lit, Hashable v, Hashable typ) => Hashable (TypedTerm typ lit v) where
    hashWithSalt s (VarT v t) = s `hashWithSalt` (0 :: Int) `hashWithSalt` v `hashWithSalt` t
    hashWithSalt s (LitT l t) = s `hashWithSalt` (1 :: Int) `hashWithSalt` l `hashWithSalt` t
    hashWithSalt s (LamT v vt body bodyt) = s `hashWithSalt` (2 :: Int) `hashWithSalt` v `hashWithSalt` vt `hashWithSalt` body `hashWithSalt` bodyt
    hashWithSalt s (AppT f x fxt) = s `hashWithSalt` (3 :: Int) `hashWithSalt` f `hashWithSalt` fxt `hashWithSalt` x 
    hashWithSalt s (OLetT v vt b1 b2 lett) = s `hashWithSalt` (4 :: Int) `hashWithSalt` v `hashWithSalt` vt `hashWithSalt` b1 `hashWithSalt` b2 `hashWithSalt` lett
    hashWithSalt s (RLetT v vt b1 b2 lett) = s `hashWithSalt` (5 :: Int) `hashWithSalt` v `hashWithSalt` vt `hashWithSalt` b1 `hashWithSalt` b2 `hashWithSalt` lett

data TypeLitO = TInt | TString
    deriving (Ord, Eq, Show, Enum, Bounded)

instance TShow TypeLitO where
    tshow TInt = "Int"
    tshow TString = "Bool"

instance Hashable TypeLitO where
    hashWithSalt s TInt = s `hashWithSalt` (0 :: Int)
    hashWithSalt s TString = s `hashWithSalt` (1 :: Int)

data LitO = LInt Int | LString Text
    deriving (Ord, Eq, Show)

instance TShow LitO where
    tshow (LInt n) = tshow n
    tshow (LString s) = tshow s

instance Hashable LitO where
    hashWithSalt s (LInt n) = s `hashWithSalt` (0 :: Int) `hashWithSalt` n
    hashWithSalt s (LString t) = s `hashWithSalt` (1 :: Int) `hashWithSalt` t

class LCTerm term where
    type TermLit term
    type TermVar term
    idTerm :: Text -> term
    varTerm :: TermVar term -> term
    litTerm :: TermLit term -> term
    intLit :: Int -> TermLit term
    stringLit :: Text -> TermLit term
    intLit' :: Int -> term
    intLit' = litTerm . (intLit @term)
    stringLit' :: Text -> term
    stringLit' = litTerm . (stringLit @term)
    absTerm :: TermVar term -> term -> term
    appTerm :: term -> term -> term

instance (IsText v) => LCTerm (Term LitO v) where
    type TermLit (Term LitO v) = LitO
    type TermVar (Term LitO v) = v
    idTerm = Var . fromText
    varTerm = Var
    litTerm = Lit
    intLit = LInt
    stringLit = LString
    absTerm = Lam
    appTerm = App

class STLCType typ where
    type TypeLit typ
    type TypeVar typ
    intLitT :: typ
    stringLitT :: typ
    funType :: typ -> typ -> typ
    varType :: TypeVar typ -> typ
    tryAppType :: typ -> typ -> Maybe typ

instance STLCType (Typ TypeLitO Text) where
    type TypeLit (Typ TypeLitO Text) = TypeLitO
    type TypeVar (Typ TypeLitO Text) = Text
    intLitT = TLit TInt
    stringLitT = TLit TString
    funType = TFun
    varType = TVar
    tryAppType (TFun arg1 b) arg2 | arg1 == arg2 = Just b
    tryAppType _ _ = Nothing

builtinPrimitives :: (STLCType typ) => [(Text, typ)]
builtinPrimitives = [
    ("addInt", funType intLitT (funType intLitT intLitT)),
    ("subInt", funType intLitT (funType intLitT intLitT)),
    ("mulInt", funType intLitT (funType intLitT intLitT)),
    ("eqInt", funType intLitT (funType intLitT intLitT)),    -- return 1 if equal, 0 otherwise
    ("concatStr", funType stringLitT (funType stringLitT stringLitT)),
    ("intToStr", funType intLitT stringLitT)
    ]

builtinSTLCPrimitives :: (IsText v, STLCType typ) => [TypedTerm typ lit v]
builtinSTLCPrimitives = map (\(name, typ) -> VarT (fromText name) typ) builtinPrimitives

typOfTTerm :: TypedTerm typ lit v -> typ
typOfTTerm (VarT _ t) = t
typOfTTerm (LitT _ t) = t
typOfTTerm (LamT _ _ _ t) = t
typOfTTerm (AppT _ _ t) = t
typOfTTerm (OLetT _ _ _ _ t) = t
typOfTTerm (RLetT _ _ _ _ t) = t

type TypWithMetaVar = Typ TypeLitO Text 
type TypNoVar = Typ TypeLitO ()

type PartialTypedTerm litT vT lit v = TypedTerm (Maybe (Typ litT vT)) lit v

instance (IsText v) => LCTerm (PartialTypedTerm TypeLitO vT LitO v) where
    type TermLit (PartialTypedTerm TypeLitO vT LitO v) = LitO
    type TermVar (PartialTypedTerm TypeLitO vT LitO v) = v
    idTerm v = VarT (fromText v) Nothing
    varTerm v = VarT v Nothing
    litTerm l = LitT l Nothing
    intLit = LInt
    stringLit = LString
    absTerm v body = LamT v Nothing body Nothing
    appTerm f x = AppT f x Nothing

annotating :: Typ litT vT -> PartialTypedTerm litT vT lit v ->  PartialTypedTerm litT vT lit v
annotating = annotating' . Just

annotating' :: Maybe (Typ litT vT) -> PartialTypedTerm litT vT lit v ->  PartialTypedTerm litT vT lit v
annotating' mt (VarT v _) = VarT v mt
annotating' mt (LitT l _) = LitT l mt
annotating' mt (LamT v vt body _) = LamT v vt (annotating' mt body) mt
annotating' mt (AppT f x _) = AppT (annotating' mt f) (annotating' mt x) mt
annotating' mt (OLetT v vt b1 b2 _) = OLetT v vt b1 b2 mt
annotating' mt (RLetT v vt b1 b2 _) = RLetT v vt b1 b2 mt

unannotating :: PartialTypedTerm litT vT lit v -> PartialTypedTerm litT vT lit v
unannotating (VarT v _) = VarT v Nothing
unannotating (LitT l _) = LitT l Nothing
unannotating (LamT v vt body _) = LamT v vt (unannotating body) Nothing
unannotating (AppT f x _) = AppT (unannotating f) (unannotating x) Nothing
unannotating (OLetT v vt b1 b2 _) = OLetT v vt b1 b2 Nothing
unannotating (RLetT v vt b1 b2 _) = RLetT v vt b1 b2 Nothing