{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeChecker
    ( TypeError(..)
    , ThrowTypeError
    , TypingCtx
    , WithTypingCtx
    , pushVar
    , lookupVar
    , builtinPrimitiveTypes
    , throwTypeError
    , litType
    , typeInfer
    , infer
    , check
    , runTypeCheckWithCtx
    , runTypeCheck
    ) where
import Ast
import Effect hiding (throw)
import Exception
import Text (TShow(..), IsText(..))
import qualified Data.Text as T
import qualified Control.Monad.Hefty as Hefty
import Control.Monad.Hefty ((:>))
import qualified Control.Monad.Hefty.Reader as Hefty
import qualified Data.Map as Map

newtype TypeError = TypeError T.Text deriving (Show, Eq, TShow, IsText)
type ThrowTypeError es = MultiThrow TypeError :> es
type TypingCtx v vT = Map.Map v (Typ TypeLitO vT)

-- | 类型检查上下文约束：需要 Reader + Local 效应提供可修改的上下文
type WithTypingCtx es v = (Hefty.Local (TypingCtx v ()) :> es, Hefty.Ask (TypingCtx v ()) :> es, Ord v)

-- | 在扩展的上下文中运行子计算（将变量 v 绑定到类型 t）
pushVar :: (Hefty.Local (TypingCtx v vT) :> es, Ord v) =>
    v -> Typ TypeLitO vT -> ParseEff s err es a -> ParseEff s err es a
pushVar v t f = ParseEff $ Hefty.local (Map.insert v t) (asEff f)

-- | 查询当前上下文中变量的类型
lookupVar :: (Hefty.Ask (TypingCtx v vT) :> es, Ord v) =>
    v -> ParseEff s err es (Maybe (Typ TypeLitO vT))
lookupVar v = do
    ctx <- ParseEff Hefty.ask
    return $ Map.lookup v ctx

-- | 内置基本类型的常量表，用于初始化类型检查上下文
builtinPrimitiveTypes :: [(T.Text, Typ TypeLitO ())]
builtinPrimitiveTypes = [
    ("addInt",    TFun (TLit TInt) (TFun (TLit TInt) (TLit TInt))),
    ("subInt",    TFun (TLit TInt) (TFun (TLit TInt) (TLit TInt))),
    ("mulInt",    TFun (TLit TInt) (TFun (TLit TInt) (TLit TInt))),
    ("eqInt",     TFun (TLit TInt) (TFun (TLit TInt) (TLit TInt))),
    ("ite",       TFun (TLit TInt) (TFun (TLit TInt) (TFun (TLit TInt) (TLit TInt)))),
    ("concatStr", TFun (TLit TString) (TFun (TLit TString) (TLit TString))),
    ("intToStr",  TFun (TLit TInt) (TLit TString))
    ]

-- | 抛出类型错误
throwTypeError :: (ThrowTypeError es) => T.Text -> ParseEff s err es a
throwTypeError msg = ParseEff $ Exception.rawThrow (TypeError msg)

-- | 字面量对应的类型
litType :: LitO -> Typ TypeLitO ()
litType (LInt _)    = TLit TInt
litType (LString _) = TLit TString

--------------------------------------------------------------------------------
-- 核心：双向类型推导/检查
--------------------------------------------------------------------------------

-- | 对 PartialTypedTerm TypeLitO () 做类型推导/检查，返回类型正确的程序或抛出异常
typeInfer :: (ThrowTypeError es, WithTypingCtx es v, TShow v) =>
    PartialTypedTerm TypeLitO () LitO v -> ParseEff s err es (
        TypedTerm (Typ TypeLitO ()) LitO v
    )
typeInfer = infer

-- | 推导项的类型（自底向上）
infer :: forall s err es v. (ThrowTypeError es, WithTypingCtx es v, TShow v) =>
    PartialTypedTerm TypeLitO () LitO v -> ParseEff s err es (
        TypedTerm (Typ TypeLitO ()) LitO v
    )
-- 变量推导
infer (VarT v Nothing) = do
    mt <- lookupVar v
    case mt of
        Just t  -> return $ VarT v t
        Nothing -> throwTypeError ("Variable not in scope: " <> tshow v)
infer (VarT v (Just t)) = do
    mt <- lookupVar v
    case mt of
        Just t' | t == t'   -> return $ VarT v t
        Just t'             -> throwTypeError
            ("Type mismatch for variable " <> tshow v
            <> ": expected " <> tshow t <> ", got " <> tshow t')
        Nothing             -> throwTypeError
            ("Variable not in scope: " <> tshow v)

-- 字面量推导
infer (LitT l Nothing) =
    return $ LitT l (litType l)
infer (LitT l (Just t)) = do
    let expected = litType l
    if t == expected
        then return $ LitT l t
        else throwTypeError
            ("Type mismatch for literal: expected "
            <> tshow expected <> ", got " <> tshow t)

-- Lambda 推导
-- 无参数注解且无整体类型 → 无法推导
infer (LamT v Nothing _ Nothing) =
    throwTypeError
        ("Cannot infer type for lambda parameter " <> tshow v
        <> " without annotation")
-- 有参数注解 → 推导体类型
infer (LamT v (Just vt) body Nothing) = do
    body' <- pushVar v vt (infer body)
    let bodyType = typOfTTerm body'
    return $ LamT v vt body' (TFun vt bodyType)
-- 无参数注解但有整体类型注解 → 从函数类型反推参数类型
infer (LamT v Nothing body (Just t)) =
    case t of
        TFun argT bodyT -> do
            body' <- pushVar v argT (check bodyT body)
            return $ LamT v argT body' t
        _ -> throwTypeError
            ("Lambda type annotation must be a function type, got: " <> tshow t)
-- 有参数注解且有整体类型注解 → 校验一致性
infer (LamT v (Just vt) body (Just t)) =
    case t of
        TFun argT bodyT
            | vt == argT -> do
                body' <- pushVar v vt (check bodyT body)
                return $ LamT v vt body' t
            | otherwise -> throwTypeError
                ("Lambda parameter type mismatch: annotated as "
                <> tshow vt <> ", but result type says " <> tshow argT)
        _ -> throwTypeError
            ("Lambda type annotation must be a function type, got: " <> tshow t)

-- 函数应用推导
infer (AppT f x Nothing) = do
    f' <- infer f
    case typOfTTerm f' of
        TFun argT retT -> do
            x' <- check argT x
            return $ AppT f' x' retT
        t -> throwTypeError
            ("Expected a function type in application, got: " <> tshow t)
infer (AppT f x (Just t)) = do
    f' <- infer f
    case typOfTTerm f' of
        TFun argT retT
            | retT == t -> do
                x' <- check argT x
                return $ AppT f' x' t
            | otherwise -> throwTypeError
                ("Type mismatch in application: expected result "
                <> tshow t <> ", got " <> tshow retT)
        _ -> throwTypeError
            ("Expected a function type in application, got: "
            <> tshow (typOfTTerm f'))

-- Let 推导
-- 无注解、无整体类型
infer (OLetT v Nothing b1 b2 Nothing) = do
    b1' <- infer b1
    let vt = typOfTTerm b1'
    b2' <- pushVar v vt (infer b2)
    return $ OLetT v vt b1' b2' (typOfTTerm b2')
-- 有变量类型注解
infer (OLetT v (Just vt) b1 b2 Nothing) = do
    b1' <- check vt b1
    b2' <- pushVar v vt (infer b2)
    return $ OLetT v vt b1' b2' (typOfTTerm b2')
-- 有整体类型注解、无变量类型注解
infer (OLetT v Nothing b1 b2 (Just t)) = do
    b1' <- infer b1
    let vt = typOfTTerm b1'
    b2' <- pushVar v vt (check t b2)
    return $ OLetT v vt b1' b2' t
-- 有变量类型注解和整体类型注解
infer (OLetT v (Just vt) b1 b2 (Just t)) = do
    b1' <- check vt b1
    b2' <- pushVar v vt (check t b2)
    return $ OLetT v vt b1' b2' t

-- 递归 Let 推导
-- 无注解 → 无法推导递归绑定
infer (RLetT v Nothing _ _ Nothing) =
    throwTypeError
        ("Cannot infer type for recursive binding " <> tshow v
        <> " without annotation")
-- 有变量类型注解
infer (RLetT v (Just vt) b1 b2 Nothing) = do
    b1' <- pushVar v vt (check vt b1)
    b2' <- pushVar v vt (infer b2)
    return $ RLetT v vt b1' b2' (typOfTTerm b2')
-- 有整体类型但无变量类型注解 → 也无法推导
infer (RLetT v Nothing _ _ (Just _)) =
    throwTypeError
        ("Cannot infer type for recursive binding " <> tshow v
        <> " without annotation")
-- 同时有变量和整体类型注解
infer (RLetT v (Just vt) b1 b2 (Just t)) = do
    b1' <- pushVar v vt (check vt b1)
    b2' <- pushVar v vt (check t b2)
    return $ RLetT v vt b1' b2' t

--------------------------------------------------------------------------------
-- 双向检查：给定期望类型，自上而下验证项
--------------------------------------------------------------------------------

-- | 检验项是否具有给定的类型（自顶向下），同时可利用期望类型推导
-- Lambda 参数类型等原本无法自底向上推导的信息。
check :: forall s err es v. (ThrowTypeError es, WithTypingCtx es v, TShow v) =>
    Typ TypeLitO () ->
    PartialTypedTerm TypeLitO () LitO v ->
    ParseEff s err es (
        TypedTerm (Typ TypeLitO ()) LitO v
    )

-- 变量检查
check expected (VarT v Nothing) = do
    mt <- lookupVar v
    case mt of
        Just t
            | t == expected -> return $ VarT v t
            | otherwise     -> throwTypeError
                ("Type mismatch for variable " <> tshow v
                <> ": expected " <> tshow expected <> ", got " <> tshow t)
        Nothing -> throwTypeError ("Variable not in scope: " <> tshow v)
check expected (VarT v (Just t))
    | t == expected = do
        mt <- lookupVar v
        case mt of
            Just t'
                | t == t'   -> return $ VarT v t
                | otherwise -> throwTypeError
                    ("Variable " <> tshow v <> " has type " <> tshow t'
                    <> " but annotated as " <> tshow t)
            Nothing -> throwTypeError ("Variable not in scope: " <> tshow v)
    | otherwise = throwTypeError
        ("Type mismatch for variable " <> tshow v
        <> ": expected " <> tshow expected <> ", annotated as " <> tshow t)

-- 字面量检查
check expected (LitT l Nothing) = do
    let t = litType l
    if t == expected
        then return $ LitT l t
        else throwTypeError
            ("Type mismatch for literal: expected "
            <> tshow expected <> ", got " <> tshow t)
check expected (LitT l (Just t))
    | t == expected = do
        let lt = litType l
        if t == lt
            then return $ LitT l t
            else throwTypeError
                ("Literal type mismatch: " <> tshow l
                <> " has type " <> tshow lt
                <> " but annotated as " <> tshow t)
    | otherwise = throwTypeError
        ("Type mismatch for literal: expected "
        <> tshow expected <> ", annotated as " <> tshow t)

-- Lambda 检查
-- 无参数注解 → 从期望类型反推参数类型
check expected (LamT v Nothing body Nothing) =
    case expected of
        TFun argT bodyT -> do
            body' <- pushVar v argT (check bodyT body)
            return $ LamT v argT body' expected
        _ -> throwTypeError
            ("Lambda expression expected to have type "
            <> tshow expected <> " but it is not a function type")
-- 有参数注解 → 校验注解与期望类型一致
check expected (LamT v (Just vt) body Nothing) =
    case expected of
        TFun argT bodyT
            | vt == argT -> do
                body' <- pushVar v vt (check bodyT body)
                return $ LamT v vt body' expected
            | otherwise -> throwTypeError
                ("Lambda parameter type mismatch: annotated as "
                <> tshow vt <> " but expected " <> tshow argT)
        _ -> throwTypeError
            ("Lambda expression expected to have type "
            <> tshow expected <> " but it is not a function type")
-- 无参数注解，有整体类型注解
check expected (LamT v Nothing body (Just t))
    | t == expected =
        case t of
            TFun argT bodyT -> do
                body' <- pushVar v argT (check bodyT body)
                return $ LamT v argT body' expected
            _ -> throwTypeError
                "Lambda type annotation must be a function type"
    | otherwise = throwTypeError
        ("Lambda type mismatch: annotated as "
        <> tshow t <> " but expected " <> tshow expected)
-- 有参数注解，有整体类型注解
check expected (LamT v (Just vt) body (Just t))
    | t == expected =
        case t of
            TFun argT bodyT
                | vt == argT -> do
                    body' <- pushVar v vt (check bodyT body)
                    return $ LamT v vt body' expected
                | otherwise -> throwTypeError
                    ("Lambda parameter type mismatch")
            _ -> throwTypeError
                ("Lambda type annotation must be a function type")
    | otherwise = throwTypeError
        ("Lambda type mismatch: annotated as "
        <> tshow t <> " but expected " <> tshow expected)

-- 函数应用检查
check expected (AppT f x Nothing) = do
    f' <- infer f
    case typOfTTerm f' of
        TFun argT retT
            | retT == expected -> do
                x' <- check argT x
                return $ AppT f' x' expected
            | otherwise -> throwTypeError
                ("Application result type mismatch: expected "
                <> tshow expected <> ", got " <> tshow retT)
        t -> throwTypeError
            ("Expected a function type in application, got: " <> tshow t)
check expected (AppT f x (Just t))
    | t == expected = do
        f' <- infer f
        case typOfTTerm f' of
            TFun argT retT
                | retT == t -> do
                    x' <- check argT x
                    return $ AppT f' x' t
                | otherwise -> throwTypeError
                    ("Application result type mismatch: "
                    <> tshow t <> " vs " <> tshow retT)
            _ -> throwTypeError
                ("Expected a function type in application, got: "
                <> tshow (typOfTTerm f'))
    | otherwise = throwTypeError
        ("Application type mismatch: annotated as "
        <> tshow t <> " but expected " <> tshow expected)

-- Let 检查
check expected (OLetT v Nothing b1 b2 Nothing) = do
    b1' <- infer b1
    let vt = typOfTTerm b1'
    b2' <- pushVar v vt (check expected b2)
    return $ OLetT v vt b1' b2' expected
check expected (OLetT v (Just vt) b1 b2 Nothing) = do
    b1' <- check vt b1
    b2' <- pushVar v vt (check expected b2)
    return $ OLetT v vt b1' b2' expected
check expected (OLetT v Nothing b1 b2 (Just _)) = do
    b1' <- infer b1
    let vt = typOfTTerm b1'
    b2' <- pushVar v vt (check expected b2)
    return $ OLetT v vt b1' b2' expected
check expected (OLetT v (Just vt) b1 b2 (Just _)) = do
    b1' <- check vt b1
    b2' <- pushVar v vt (check expected b2)
    return $ OLetT v vt b1' b2' expected

-- 递归 Let 检查
check _ (RLetT v Nothing _ _ Nothing) =
    throwTypeError
        ("Cannot infer type for recursive binding " <> tshow v
        <> " without annotation")
check expected (RLetT v (Just vt) b1 b2 Nothing) = do
    b1' <- pushVar v vt (check vt b1)
    b2' <- pushVar v vt (check expected b2)
    return $ RLetT v vt b1' b2' expected
check _ (RLetT v Nothing _ _ (Just _)) =
    throwTypeError
        ("Cannot infer type for recursive binding " <> tshow v
        <> " without annotation")
check expected (RLetT v (Just vt) b1 b2 (Just t))
    | t == expected = do
        b1' <- pushVar v vt (check vt b1)
        b2' <- pushVar v vt (check expected b2)
        return $ RLetT v vt b1' b2' expected
    | otherwise = throwTypeError
        ("Recursive let type mismatch: annotated as "
        <> tshow t <> " but expected " <> tshow expected)

--------------------------------------------------------------------------------
-- 运行类型检查的辅助函数
--------------------------------------------------------------------------------

-- | 使用指定的初始上下文运行类型推导/检查
runTypeCheckWithCtx :: forall s err es v a.
    TypingCtx v () ->
    ParseEff s err (Hefty.Local (TypingCtx v ()) : Hefty.Ask (TypingCtx v ()) : es) a ->
    ParseEff s err es a
runTypeCheckWithCtx initCtx (ParseEff p) =
    ParseEff $ Hefty.runReader initCtx p

-- | 使用内置基本类型初始化上下文并运行类型推导/检查。
-- 内置基本类型包括 addInt, subInt, mulInt, eqInt, concatStr, intToStr。
runTypeCheck :: forall s err es v a. (Ord v, IsText v) =>
    ParseEff s err (Hefty.Local (TypingCtx v ()) : Hefty.Ask (TypingCtx v ()) : es) a ->
    ParseEff s err es a
runTypeCheck (ParseEff p) =
    let initCtx = Map.fromList
            [(fromText name, typ) | (name, typ) <- builtinPrimitiveTypes]
    in ParseEff $ Hefty.runReader initCtx p
