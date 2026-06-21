module InterpreterSpec where
import Test.Hspec
import Interpreter
import Ast
import Parser (TrmVar(..))
import TypeCheckerSpec (parseAndCheck)
import qualified Data.Text as T
import qualified Data.Tree as Tree

--------------------------------------------------------------------------------
-- 测试辅助函数
--------------------------------------------------------------------------------

-- | 运行求值，提取成功结果或失败
expectEval :: Either (Tree.Forest RuntimeError) Value -> Value
expectEval (Right val) = val
expectEval (Left errs) = error $ "Runtime error: " ++ show errs

shouldEvalToInt :: TypedTerm (Typ TypeLitO ()) LitO TrmVar -> Int -> Expectation
shouldEvalToInt term n =
    case runEval term of
        Right (VInt m) -> m `shouldBe` n
        Right other    -> expectationFailure $ "Expected VInt " ++ show n ++ " but got " ++ show other
        Left errs      -> expectationFailure $ "Runtime error: " ++ show errs

shouldEvalToStr :: TypedTerm (Typ TypeLitO ()) LitO TrmVar -> T.Text -> Expectation
shouldEvalToStr term s =
    case runEval term of
        Right (VString t) -> t `shouldBe` s
        Right other       -> expectationFailure $ "Expected VString " ++ show s ++ " but got " ++ show other
        Left errs         -> expectationFailure $ "Runtime error: " ++ show errs

shouldEvalToClosure :: TypedTerm (Typ TypeLitO ()) LitO TrmVar -> Expectation
shouldEvalToClosure term =
    case runEval term of
        Right (VClosure {}) -> return ()
        Right other         -> expectationFailure $ "Expected closure but got " ++ show other
        Left errs           -> expectationFailure $ "Runtime error: " ++ show errs

-- | 求值并断言抛出运行时错误
shouldEvalToRuntimeError :: TypedTerm (Typ TypeLitO ()) LitO TrmVar -> Expectation
shouldEvalToRuntimeError term =
    case runEval term of
        Left _   -> return ()
        Right v  -> expectationFailure $ "Expected runtime error but got " ++ show v

parseCheckEval :: T.Text -> Value
parseCheckEval src =
    case parseAndCheck src of
        Left parseErr -> error $ "Parse error: " ++ T.unpack parseErr
        Right (Left tcErrs) -> error $ "Type error: " ++ show tcErrs
        Right (Right term) -> expectEval (runEval term)

shouldParseCheckEvalTo :: T.Text -> Value -> Expectation
shouldParseCheckEvalTo src expected =
    valueEq (parseCheckEval src) expected `shouldBe` True

valueEq :: Value -> Value -> Bool
valueEq (VInt a) (VInt b)       = a == b
valueEq (VString a) (VString b) = a == b
valueEq (VClosure _ _ _) (VClosure _ _ _) = True
valueEq (VNative _) (VNative _)           = True
valueEq (VThunk _ _) (VThunk _ _)         = True
valueEq _ _                               = False

--------------------------------------------------------------------------------
-- 类型构建辅助
--------------------------------------------------------------------------------

intT, stringT :: Typ TypeLitO ()
intT    = TLit TInt
stringT = TLit TString

funT :: Typ TypeLitO () -> Typ TypeLitO () -> Typ TypeLitO ()
funT = TFun

intToInt :: Typ TypeLitO ()
intToInt = funT intT intT

intToIntToInt :: Typ TypeLitO ()
intToIntToInt = funT intT (funT intT intT)

intToIntToIntToInt :: Typ TypeLitO ()
intToIntToIntToInt = funT intT (funT intT (funT intT intT))

--------------------------------------------------------------------------------
-- 项构建辅助
--------------------------------------------------------------------------------

mkVar :: T.Text -> Typ TypeLitO () -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkVar name ty = VarT (TrmVar name) ty

mkInt :: Int -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkInt n = LitT (LInt n) intT

mkStr :: T.Text -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkStr s = LitT (LString s) stringT

mkLam :: T.Text -> Typ TypeLitO () -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkLam name paramTy body =
    LamT (TrmVar name) paramTy body (funT paramTy (typOfTTerm body))

mkApp :: TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> Typ TypeLitO ()
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkApp f x retTy = AppT f x retTy

mkLet :: T.Text -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkLet name b1 b2 = OLetT (TrmVar name) (typOfTTerm b1) b1 b2 (typOfTTerm b2)

mkRLet :: T.Text -> Typ TypeLitO () -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkRLet name ty b1 b2 = RLetT (TrmVar name) ty b1 b2 (typOfTTerm b2)

mkBuiltin2 :: T.Text -> Typ TypeLitO ()
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> Typ TypeLitO ()
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkBuiltin2 name fnTy a1 a2 retTy =
    mkApp (mkApp (mkVar name fnTy) a1 (funT intT retTy)) a2 retTy

mkBuiltin1 :: T.Text -> Typ TypeLitO ()
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> Typ TypeLitO ()
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkBuiltin1 name fnTy arg retTy = mkApp (mkVar name fnTy) arg retTy

-- | 三元内置函数：ite cond thenB elseB
mkIte :: TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
    -> TypedTerm (Typ TypeLitO ()) LitO TrmVar
mkIte cond thenB elseB =
    mkApp (mkApp (mkApp (mkVar "ite" intToIntToIntToInt) cond
        (funT intT (funT intT intT))) thenB (funT intT intT)) elseB intT

--------------------------------------------------------------------------------
-- 测试用例
--------------------------------------------------------------------------------

literalISpec :: Spec
literalISpec = describe "Literal evaluation" $ do
    it "evaluates integer literal" $
        shouldEvalToInt (mkInt 42) 42
    it "evaluates string literal" $
        shouldEvalToStr (mkStr "hello") "hello"

lambdaISpec :: Spec
lambdaISpec = describe "Lambda evaluation" $ do
    it "evaluates to closure" $
        shouldEvalToClosure (mkLam "x" intT (mkVar "x" intT))

applicationISpec :: Spec
applicationISpec = describe "Application evaluation" $ do
    it "applies identity to integer" $
        shouldEvalToInt (mkApp (mkLam "x" intT (mkVar "x" intT)) (mkInt 42) intT) 42
    it "applies constant function" $
        shouldEvalToInt (mkApp (mkLam "x" intT (mkInt 42)) (mkInt 99) intT) 42
    it "evaluates nested application (selects first arg)" $
        shouldEvalToInt
            (mkApp (mkApp (mkLam "x" intT (mkLam "y" intT (mkVar "x" intT))) (mkInt 1) (funT intT intT)) (mkInt 2) intT)
            1

builtinISpec :: Spec
builtinISpec = describe "Built-in function evaluation" $ do
    it "evaluates addInt 1 2 = 3" $
        shouldEvalToInt (mkBuiltin2 "addInt" intToIntToInt (mkInt 1) (mkInt 2) intT) 3
    it "evaluates subInt 10 3 = 7" $
        shouldEvalToInt (mkBuiltin2 "subInt" intToIntToInt (mkInt 10) (mkInt 3) intT) 7
    it "evaluates mulInt 4 5 = 20" $
        shouldEvalToInt (mkBuiltin2 "mulInt" intToIntToInt (mkInt 4) (mkInt 5) intT) 20
    it "evaluates eqInt 7 7 = 1" $
        shouldEvalToInt (mkBuiltin2 "eqInt" intToIntToInt (mkInt 7) (mkInt 7) intT) 1
    it "evaluates eqInt 7 8 = 0" $
        shouldEvalToInt (mkBuiltin2 "eqInt" intToIntToInt (mkInt 7) (mkInt 8) intT) 0
    it "evaluates concatStr \"hello\" \"world\"" $
        shouldEvalToStr
            (mkApp (mkApp (mkVar "concatStr" (funT stringT (funT stringT stringT)))
                (mkStr "hello") (funT stringT stringT)) (mkStr "world") stringT)
            "helloworld"
    it "evaluates intToStr 123 = \"123\"" $
        shouldEvalToStr (mkBuiltin1 "intToStr" (funT intT stringT) (mkInt 123) stringT) "123"
    it "evaluates ite with true condition" $
        -- ite 1 42 99  =  42
        shouldEvalToInt (mkIte (mkInt 1) (mkInt 42) (mkInt 99)) 42
    it "evaluates ite with false condition" $
        -- ite 0 42 99  =  99
        shouldEvalToInt (mkIte (mkInt 0) (mkInt 42) (mkInt 99)) 99
    it "ite is lazy: unevaluated branch is not forced" $
        -- ite 1 42 undefinedVar  —  true branch, else branch never evaluated
        shouldEvalToInt (mkIte (mkInt 1) (mkInt 42) (mkVar "undefinedVar" intT)) 42

letISpec :: Spec
letISpec = describe "Let binding evaluation" $ do
    it "evaluates simple let" $
        shouldEvalToInt (mkLet "x" (mkInt 1) (mkVar "x" intT)) 1
    it "evaluates nested let" $
        shouldEvalToInt (mkLet "x" (mkInt 1) (mkLet "y" (mkInt 2) (mkVar "x" intT))) 1
    it "evaluates let with function" $
        shouldEvalToInt
            (mkLet "f" (mkLam "x" intT (mkVar "x" intT))
                (mkApp (mkVar "f" intToInt) (mkInt 42) intT))
            42

lazyISpec :: Spec
lazyISpec = describe "Lazy evaluation" $ do
    it "does not evaluate unused let binding" $
        shouldEvalToInt (mkLet "x" (mkVar "undefinedVar" intT) (mkInt 42)) 42
    it "short-circuits in constant function" $
        shouldEvalToInt (mkApp (mkLam "x" intT (mkInt 42)) (mkVar "undefinedVar" intT) intT) 42

recLetISpec :: Spec
recLetISpec = describe "Recursive let evaluation" $ do
    it "evaluates recursive identity function" $
        -- let rec f :: Int -> Int = \x :: Int -> x in f 42
        shouldEvalToInt
            (mkRLet "f" intToInt (mkLam "x" intT (mkVar "x" intT))
                (mkApp (mkVar "f" intToInt) (mkInt 42) intT))
            42
    it "evaluates nested recursive calls" $
        -- let rec f :: Int -> Int = \x :: Int -> x in f (f 5)
        shouldEvalToInt
            (mkRLet "f" intToInt (mkLam "x" intT (mkVar "x" intT))
                (mkApp (mkVar "f" intToInt)
                    (mkApp (mkVar "f" intToInt) (mkInt 5) intT) intT))
            5
    it "evaluates factorial (real recursion with termination)" $
        -- rec fact = \n -> ite (eqInt n 0) 1 (mulInt n (fact (subInt n 1)))
        -- fact 5 = 120
        let z     = mkInt 0
            one   = mkInt 1
            nVar  = mkVar "n" intT
            body = mkLam "n" intT $
                mkIte
                    (mkBuiltin2 "eqInt" intToIntToInt nVar z intT)
                    one
                    (mkBuiltin2 "mulInt" intToIntToInt nVar
                        (mkApp (mkVar "fact" intToInt)
                            (mkBuiltin2 "subInt" intToIntToInt nVar one intT) intT)
                        intT)
        in shouldEvalToInt
            (mkRLet "fact" intToInt body (mkApp (mkVar "fact" intToInt) (mkInt 5) intT))
            120
    it "evaluates sum up to n (real recursion with termination)" $
        -- rec sum = \n -> ite (eqInt n 0) 0 (addInt n (sum (subInt n 1)))
        -- sum 3 = 3 + 2 + 1 + 0 = 6
        let z    = mkInt 0
            nVar = mkVar "n" intT
            body = mkLam "n" intT $
                mkIte
                    (mkBuiltin2 "eqInt" intToIntToInt nVar z intT)
                    z
                    (mkBuiltin2 "addInt" intToIntToInt nVar
                        (mkApp (mkVar "sum" intToInt)
                            (mkBuiltin2 "subInt" intToIntToInt nVar (mkInt 1) intT) intT)
                        intT)
        in shouldEvalToInt
            (mkRLet "sum" intToInt body (mkApp (mkVar "sum" intToInt) (mkInt 3) intT))
            6
    it "recursive function with larger recursion depth" $
        -- rec sum = \n -> ite (eqInt n 0) 0 (addInt n (sum (subInt n 1)))
        -- sum 10 = 55
        let z    = mkInt 0
            nVar = mkVar "n" intT
            body = mkLam "n" intT $
                mkIte
                    (mkBuiltin2 "eqInt" intToIntToInt nVar z intT)
                    z
                    (mkBuiltin2 "addInt" intToIntToInt nVar
                        (mkApp (mkVar "sum" intToInt)
                            (mkBuiltin2 "subInt" intToIntToInt nVar (mkInt 1) intT) intT)
                        intT)
        in shouldEvalToInt
            (mkRLet "sum" intToInt body (mkApp (mkVar "sum" intToInt) (mkInt 10) intT))
            55

--------------------------------------------------------------------------------
-- 异常安全测试
--------------------------------------------------------------------------------

errorISpec :: Spec
errorISpec = describe "Runtime error handling" $ do
    it "throws error for unbound variable" $
        shouldEvalToRuntimeError (mkVar "undefinedVar" intT)
    it "throws error for application of non-function" $
        shouldEvalToRuntimeError (mkApp (mkInt 1) (mkInt 2) intT)
    it "evaluates thunk that references unbound variable only when forced" $
        -- let x = undefinedVar in 42  — 不访问 x，不应报错
        shouldEvalToInt (mkLet "x" (mkVar "undefinedVar" intT) (mkInt 42)) 42
    it "throws error when thunk that references unbound variable is forced" $
        -- let x = undefinedVar in x  — 访问 x，应报错
        shouldEvalToRuntimeError (mkLet "x" (mkVar "undefinedVar" intT) (mkVar "x" intT))

--------------------------------------------------------------------------------
-- 端到端集成测试
--------------------------------------------------------------------------------

integrationISpec :: Spec
integrationISpec = describe "Parser + TypeChecker + Interpreter integration" $ do
    it "evaluates integer literal end-to-end" $
        shouldParseCheckEvalTo "42" (VInt 42)
    it "evaluates string literal end-to-end" $
        case parseCheckEval "\"hello\"" of
            VString s -> shouldBe s "hello"
            other     -> expectationFailure $ "Expected VString but got " ++ show other
    it "evaluates simple let end-to-end" $
        shouldParseCheckEvalTo "let x = 1 in x" (VInt 1)
    it "evaluates annotated expression end-to-end" $
        shouldParseCheckEvalTo "1 :: Int" (VInt 1)
    it "evaluates nested let end-to-end" $
        shouldParseCheckEvalTo "let x = 1 in let y = \"hello\" in x" (VInt 1)
