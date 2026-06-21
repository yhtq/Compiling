{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Interpreter
    ( RuntimeError(..)
    , Value(..)
    , Env
    , emptyEnv
    , extendEnv
    , builtinEnv
    , eval
    , runEval
    ) where
import Ast
import Parser (TrmVar(..))
import Exception (MultiThrow, translateToTree, rawThrow)
import Text (TShow(..), IsText(..))
import qualified Data.Text as T
import qualified Control.Monad.Hefty as Hefty
import Control.Monad.Hefty ((:>))
import qualified Data.Map as Map
import qualified Data.Tree as Tree

--------------------------------------------------------------------------------
-- 运行时错误
--------------------------------------------------------------------------------

newtype RuntimeError = RuntimeError T.Text
    deriving (Show, Eq, TShow, IsText)

throwRuntimeError :: (MultiThrow RuntimeError :> es) => T.Text -> Hefty.Eff es a
throwRuntimeError msg = rawThrow (RuntimeError msg)

--------------------------------------------------------------------------------
-- 值类型
--------------------------------------------------------------------------------

-- | 解释器求值结果的值类型。
--   @VThunk@ 显式表示悬置计算，@VNative@ 中的函数可运行于任意支持异常的效应栈。
data Value
    = VInt Int
    | VString T.Text
    | VClosure TrmVar (TypedTerm (Typ TypeLitO ()) LitO TrmVar) Env
    | VNative (forall es. (MultiThrow RuntimeError :> es) => Value -> Hefty.Eff es Value)
    | VThunk Env (TypedTerm (Typ TypeLitO ()) LitO TrmVar)

instance Show Value where
    show (VInt n)         = "VInt " ++ show n
    show (VString s)      = "VString " ++ show s
    show (VClosure v _ _) = "VClosure <" ++ show v ++ ">"
    show (VNative _)      = "VNative <function>"
    show (VThunk _ _)     = "VThunk <suspended>"

--------------------------------------------------------------------------------
-- 环境
--------------------------------------------------------------------------------

newtype Env = Env (Map.Map TrmVar Value)
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty

extendEnv :: Env -> TrmVar -> Value -> Env
extendEnv (Env m) v val = Env (Map.insert v val m)

lookupEnv :: Env -> TrmVar -> Maybe Value
lookupEnv (Env m) v = Map.lookup v m

--------------------------------------------------------------------------------
-- 强制求值
--------------------------------------------------------------------------------

-- | 强制求值一个可能为 @VThunk@ 的值（用于内置函数在需要时驱动求值）
forceValue :: (MultiThrow RuntimeError :> es) => Value -> Hefty.Eff es Value
forceValue (VThunk env body) = eval env body
forceValue val               = return val

--------------------------------------------------------------------------------
-- 内置基本函数
--------------------------------------------------------------------------------

builtinEnv :: Env
builtinEnv = Env $ Map.fromList
    [ (TrmVar "addInt",    binIntOp (+))
    , (TrmVar "subInt",    binIntOp (-))
    , (TrmVar "mulInt",    binIntOp (*))
    , (TrmVar "eqInt",     binIntCmp (==))
    , (TrmVar "ite",       itePrim)
    , (TrmVar "concatStr", binStrOp (<>))
    , (TrmVar "intToStr",  VNative $ \x -> do
          x' <- forceValue x
          case x' of
              VInt n -> return (VString (T.pack (show n)))
              _      -> throwRuntimeError "intToStr: expected Int")
    ]
  where
    -- 二元整数运算：两个参数都是惰性的，在需要时 force
    binIntOp :: (Int -> Int -> Int) -> Value
    binIntOp op = VNative $ \x ->
        return $ VNative $ \y -> do
            x' <- forceValue x
            y' <- forceValue y
            case (x', y') of
                (VInt a, VInt b) -> return (VInt (op a b))
                _                -> throwRuntimeError "binIntOp: type error"

    binIntCmp :: (Int -> Int -> Bool) -> Value
    binIntCmp cmp = VNative $ \x ->
        return $ VNative $ \y -> do
            x' <- forceValue x
            y' <- forceValue y
            case (x', y') of
                (VInt a, VInt b) -> return (VInt (if cmp a b then 1 else 0))
                _                -> throwRuntimeError "binIntCmp: type error"

    binStrOp :: (T.Text -> T.Text -> T.Text) -> Value
    binStrOp op = VNative $ \x ->
        return $ VNative $ \y -> do
            x' <- forceValue x
            y' <- forceValue y
            case (x', y') of
                (VString a, VString b) -> return (VString (op a b))
                _                      -> throwRuntimeError "binStrOp: type error"

    -- ite :: Int -> Int -> Int -> Int
    -- 若条件非零返回 thenBranch，否则返回 elseBranch。
    -- 关键：只有被选中的分支会被 forceValue 求值，另一分支保持惰性。
    itePrim :: Value
    itePrim = VNative $ \cond ->
        return $ VNative $ \thenB ->
        return $ VNative $ \elseB -> do
            c <- forceValue cond
            case c of
                VInt 0 -> forceValue elseB
                VInt _ -> forceValue thenB
                _      -> throwRuntimeError "ite: expected Int condition"

--------------------------------------------------------------------------------
-- 惰性求值器（异常安全版）
--------------------------------------------------------------------------------

-- | 在当前环境中惰性求值一个完全类型化的项。
eval :: forall es. (MultiThrow RuntimeError :> es) =>
    Env -> TypedTerm (Typ TypeLitO ()) LitO TrmVar -> Hefty.Eff es Value
eval env (VarT v _) =
    case lookupEnv env v of
        Just (VThunk env' body) -> eval env' body
        Just val                -> return val
        Nothing                 -> throwRuntimeError ("Unbound variable: " <> tshow v)

eval _ (LitT (LInt n) _)    = return (VInt n)
eval _ (LitT (LString s) _) = return (VString s)

eval env (LamT v _ body _)  = return (VClosure v body env)

eval env (AppT f x _) = do
    fVal <- eval env f
    apply fVal x env

eval env (OLetT v _ b1 b2 _) =
    eval (extendEnv env v (VThunk env b1)) b2

eval env (RLetT v _ b1 b2 _) =
    let env' = extendEnv env v (VThunk env' b1)
    in eval env' b2

-- | 将函数值应用到参数。
--
-- 对于闭包：参数包装为 @VThunk@（惰性）；
-- 对于原生函数：参数同样包装为 @VThunk@，由内置函数自行按需 @forceValue@。
-- 这使得 @ite@ 可以惰性选择分支，支持真正的递归终止。
apply :: (MultiThrow RuntimeError :> es) =>
    Value -> TypedTerm (Typ TypeLitO ()) LitO TrmVar -> Env -> Hefty.Eff es Value
apply (VClosure v body closEnv) x env =
    eval (extendEnv closEnv v (VThunk env x)) body
apply (VNative fn) x _env =
    -- 原生函数的参数以 thunk 形式传入，由函数内部按需 force
    fn (VThunk _env x)
apply (VThunk env' body) x env = do
    fVal' <- eval env' body
    apply fVal' x env
apply other _ _ =
    throwRuntimeError ("Application of non-function: " <> T.pack (show other))

--------------------------------------------------------------------------------
-- 便捷入口
--------------------------------------------------------------------------------

-- | 在含内置函数的初始环境中运行求值，返回结果或运行时错误森林
runEval :: TypedTerm (Typ TypeLitO ()) LitO TrmVar
        -> Either (Tree.Forest RuntimeError) Value
runEval term = Hefty.runPure $ handleErrors $ eval builtinEnv term
  where
    handleErrors :: Hefty.Eff '[MultiThrow RuntimeError] a
                 -> Hefty.Eff '[] (Either (Tree.Forest RuntimeError) a)
    handleErrors = Hefty.interpretBy (pure . Right)
        (\e _k -> pure $ Left (translateToTree e))
