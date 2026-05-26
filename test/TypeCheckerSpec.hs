module TypeCheckerSpec where
import Test.Hspec
import TypeChecker
import Ast
import Parser (TrmVar(..), TyVar, parseExp, PTerm)
import ParserSpec (simpleParser)
import Effect (ParseEff(..), asEff)
import Exception (translateToTree, MultiThrow)
import Text (TShow(..), IsText(..))
import qualified Control.Monad.Hefty as Hefty
import qualified Control.Monad.Hefty.Reader as Hefty
import qualified Data.Tree as T
import qualified Data.Text as T
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- 测试辅助函数
--------------------------------------------------------------------------------

-- | 纯函数式运行类型推导（包含内置基本类型上下文）
runTC :: PartialTypedTerm TypeLitO () LitO TrmVar
    -> Either (T.Forest TypeError) (TypedTerm (Typ TypeLitO ()) LitO TrmVar)
runTC term =
    let initCtx = Map.fromList
            [(fromText name, typ) | (name, typ) <- builtinPrimitiveTypes]
        p :: Hefty.Eff (TyEff TrmVar) (TypedTerm (Typ TypeLitO ()) LitO TrmVar)
        p = asEff (typeInfer term)
    in Hefty.runPure $ handleMultiThrow $ Hefty.runReader initCtx p

-- | 使用自定义上下文运行类型推导
runTCWithCtx :: TypingCtx TrmVar ()
    -> PartialTypedTerm TypeLitO () LitO TrmVar
    -> Either (T.Forest TypeError) (TypedTerm (Typ TypeLitO ()) LitO TrmVar)
runTCWithCtx ctx term =
    let p :: Hefty.Eff (TyEff TrmVar) (TypedTerm (Typ TypeLitO ()) LitO TrmVar)
        p = asEff (typeInfer term)
    in Hefty.runPure $ handleMultiThrow $ Hefty.runReader ctx p

-- | 类型检查效应栈（固定顺序）
type TyEff v = '[Hefty.Local (TypingCtx v ()), Hefty.Ask (TypingCtx v ()), MultiThrow TypeError]

-- | 处理 MultiThrow 效应，将其转换为 Either
handleMultiThrow :: Hefty.Eff '[MultiThrow TypeError] a -> Hefty.Eff '[] (Either (T.Forest TypeError) a)
handleMultiThrow = Hefty.interpretBy (pure . Right) (\e _k -> pure $ Left (translateToTree e))

-- | 断言类型推导成功且结果类型匹配
shouldTypeCheck :: PartialTypedTerm TypeLitO () LitO TrmVar
    -> Typ TypeLitO ()
    -> Expectation
shouldTypeCheck term expectedType =
    case runTC term of
        Left errs -> expectationFailure $
            "Expected type " ++ T.unpack (tshow expectedType) ++
            " but got error: " ++ show errs
        Right typedTerm ->
            typOfTTerm typedTerm `shouldBe` expectedType

-- | 断言类型推导失败
shouldFailTypeCheck :: PartialTypedTerm TypeLitO () LitO TrmVar -> Expectation
shouldFailTypeCheck term =
    case runTC term of
        Left _   -> return ()
        Right tm -> expectationFailure $
            "Expected type error but got: " ++ T.unpack (tshow (typOfTTerm tm))

-- | 断言在自定义上下文中类型推导成功
shouldTypeCheckWithCtx :: TypingCtx TrmVar ()
    -> PartialTypedTerm TypeLitO () LitO TrmVar
    -> Typ TypeLitO ()
    -> Expectation
shouldTypeCheckWithCtx ctx term expectedType =
    case runTCWithCtx ctx term of
        Left errs -> expectationFailure $
            "Expected type " ++ T.unpack (tshow expectedType) ++
            " but got error: " ++ show errs
        Right typedTerm ->
            typOfTTerm typedTerm `shouldBe` expectedType

-- | 辅助谓词
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

--------------------------------------------------------------------------------
-- 类型构建辅助
--------------------------------------------------------------------------------

intT, stringT :: Typ TypeLitO ()
intT    = TLit TInt
stringT = TLit TString

funT :: Typ TypeLitO () -> Typ TypeLitO () -> Typ TypeLitO ()
funT = TFun

intToInt, intToIntToInt :: Typ TypeLitO ()
intToInt      = funT intT intT          -- Int -> Int
intToIntToInt = funT intT intToInt      -- Int -> Int -> Int

--------------------------------------------------------------------------------
-- 项构建辅助
--------------------------------------------------------------------------------

mkVar :: T.Text -> PartialTypedTerm TypeLitO () LitO TrmVar
mkVar name = VarT (TrmVar name) Nothing

mkInt :: Int -> PartialTypedTerm TypeLitO () LitO TrmVar
mkInt n = LitT (LInt n) Nothing

mkStr :: T.Text -> PartialTypedTerm TypeLitO () LitO TrmVar
mkStr s = LitT (LString s) Nothing

mkLam :: T.Text -> Typ TypeLitO () -> PartialTypedTerm TypeLitO () LitO TrmVar
    -> PartialTypedTerm TypeLitO () LitO TrmVar
mkLam name ty body = LamT (TrmVar name) (Just ty) body Nothing

mkApp :: PartialTypedTerm TypeLitO () LitO TrmVar
    -> PartialTypedTerm TypeLitO () LitO TrmVar
    -> PartialTypedTerm TypeLitO () LitO TrmVar
mkApp f x = AppT f x Nothing

mkLet :: T.Text -> PartialTypedTerm TypeLitO () LitO TrmVar
    -> PartialTypedTerm TypeLitO () LitO TrmVar
    -> PartialTypedTerm TypeLitO () LitO TrmVar
mkLet name b1 b2 = OLetT (TrmVar name) Nothing b1 b2 Nothing

mkRLet :: T.Text -> Typ TypeLitO () -> PartialTypedTerm TypeLitO () LitO TrmVar
    -> PartialTypedTerm TypeLitO () LitO TrmVar
    -> PartialTypedTerm TypeLitO () LitO TrmVar
mkRLet name ty b1 b2 = RLetT (TrmVar name) (Just ty) b1 b2 Nothing

--------------------------------------------------------------------------------
-- 测试用例
--------------------------------------------------------------------------------

literalSpec :: Spec
literalSpec = describe "Literal type inference" $ do
    it "infers Int for integer literal" $
        shouldTypeCheck (mkInt 42) intT
    it "infers String for string literal" $
        shouldTypeCheck (mkStr "hello") stringT
    it "checks annotated Int literal" $
        shouldTypeCheck (LitT (LInt 1) (Just intT)) intT
    it "rejects mismatched literal annotation" $
        shouldFailTypeCheck (LitT (LInt 1) (Just stringT))

variableSpec :: Spec
variableSpec = describe "Variable type inference" $ do
    let ctxX = Map.singleton (TrmVar "x") intT
    it "looks up variable in context" $
        shouldTypeCheckWithCtx ctxX (mkVar "x") intT
    it "rejects variable not in scope" $
        shouldFailTypeCheck (mkVar "undefinedVar")
    it "checks annotated variable matches context" $
        isRight (runTCWithCtx ctxX (VarT (TrmVar "x") (Just intT)))
            `shouldBe` True
    it "rejects annotated variable with wrong annotation" $
        isRight (runTCWithCtx ctxX (VarT (TrmVar "x") (Just stringT)))
            `shouldBe` False

lambdaSpec :: Spec
lambdaSpec = describe "Lambda type inference" $ do
    it "infers lambda type with parameter annotation" $
        shouldTypeCheck (mkLam "x" intT (mkVar "x")) (funT intT intT)
    it "infers nested lambda" $
        shouldTypeCheck (mkLam "x" intT (mkLam "y" stringT (mkVar "x")))
            (funT intT (funT stringT intT))
    it "rejects lambda without parameter annotation" $
        shouldFailTypeCheck (LamT (TrmVar "x") Nothing (mkVar "x") Nothing)
    it "infers lambda from overall type annotation" $
        shouldTypeCheck
            (LamT (TrmVar "x") Nothing (mkVar "x") (Just (funT intT intT)))
            (funT intT intT)
    it "rejects lambda with mismatched param annotation vs overall type" $
        shouldFailTypeCheck
            (LamT (TrmVar "x") (Just stringT) (mkVar "x") (Just (funT intT intT)))

applicationSpec :: Spec
applicationSpec = describe "Application type inference" $ do
    let lamId = mkLam "x" intT (mkVar "x")  -- \x::Int -> x
    it "infers simple application" $
        -- (\x :: Int -> x) 42  :  Int
        shouldTypeCheck (mkApp lamId (mkInt 42)) intT
    it "infers addInt application (builtin)" $
        -- addInt 1 2
        shouldTypeCheck (mkApp (mkApp (mkVar "addInt") (mkInt 1)) (mkInt 2)) intT
    it "rejects application to non-function" $
        -- 42 "hello"
        shouldFailTypeCheck (mkApp (mkInt 42) (mkStr "hello"))
    it "rejects application with argument type mismatch" $
        -- addInt "hello" 1
        shouldFailTypeCheck
            (mkApp (mkApp (mkVar "addInt") (mkStr "hello")) (mkInt 1))
    it "rejects application with wrong return type annotation" $
        -- addInt 1 2 :: String
        shouldFailTypeCheck
            (AppT (mkApp (mkVar "addInt") (mkInt 1)) (mkInt 2) (Just stringT))

letSpecTC :: Spec
letSpecTC = describe "Let binding type inference" $ do
    it "infers simple let binding" $
        -- let x = 1 in x  :  Int
        shouldTypeCheck (mkLet "x" (mkInt 1) (mkVar "x")) intT
    it "infers let with lambda" $ do
        -- let f = \x :: Int -> x in f 1  :  Int
        let lamI = mkLam "x" intT (mkVar "x")
            body = mkApp (mkLet "f" lamI (mkVar "f")) (mkInt 42)
        shouldTypeCheck body intT
    it "infers nested let bindings" $
        -- let x = 1 in let y = "hello" in x  :  Int
        shouldTypeCheck
            (mkLet "x" (mkInt 1) (mkLet "y" (mkStr "hello") (mkVar "x")))
            intT
    it "rejects let with mismatched type annotation" $
        shouldFailTypeCheck
            (OLetT (TrmVar "x") (Just stringT) (mkInt 1) (mkVar "x") Nothing)

recLetSpec :: Spec
recLetSpec = describe "Recursive let binding type inference" $ do
    let lamI = mkLam "x" intT (mkVar "x")  -- \x::Int -> x
    it "infers recursive let with type annotation" $
        -- let rec f :: Int -> Int = \x :: Int -> x in f 1  :  Int
        shouldTypeCheck
            (mkApp (mkRLet "f" intToInt lamI (mkVar "f")) (mkInt 42))
            intT
    it "infers recursive let with full type annotation" $
        -- let rec f :: Int -> Int = \x :: Int -> x in f  :  Int -> Int
        shouldTypeCheck (mkRLet "f" intToInt lamI (mkVar "f")) intToInt
    it "rejects recursive let without type annotation" $
        shouldFailTypeCheck
            (RLetT (TrmVar "f") Nothing (mkVar "f") (mkVar "f") Nothing)
    it "rejects recursive let with wrong type" $
        shouldFailTypeCheck
            (RLetT (TrmVar "f") (Just stringT) lamI (mkVar "f") Nothing)

complexSpec :: Spec
complexSpec = describe "Complex expression type inference" $ do
    it "infers ((\\x :: Int -> x) 1)" $
        shouldTypeCheck (mkApp (mkLam "x" intT (mkVar "x")) (mkInt 1)) intT
    it "infers higher-order application" $ do
        -- (\f :: Int -> Int -> f 1) (\x :: Int -> x)  :  Int
        let inner = mkLam "x" intT (mkVar "x")
            outer = mkLam "f" intToInt (mkApp (mkVar "f") (mkInt 1))
            term = mkApp outer inner
        shouldTypeCheck term intT
    it "rejects mismatched higher-order type" $ do
        -- (\f :: String -> String -> f 1) won't type check
        let inner = mkLam "x" stringT (mkVar "x")
            outer = mkLam "f" (funT stringT stringT) (mkApp (mkVar "f") (mkInt 1))
            term = mkApp outer inner
        shouldFailTypeCheck term

--------------------------------------------------------------------------------
-- Parser ↔ TypeChecker 集成测试
--------------------------------------------------------------------------------

-- | 将 Parser 产出的 PTerm（类型含 TyVar）转换为 TypeChecker 接受的
-- PartialTypedTerm（类型含 ()，即无类型变量）。
-- TyVar 在此简单类型系统中不应出现，统一抹除为 ()。
eraseTy :: PTerm -> PartialTypedTerm TypeLitO () LitO TrmVar
eraseTy (VarT v t)       = VarT v (eraseTyM t)
eraseTy (LitT l t)       = LitT l (eraseTyM t)
eraseTy (LamT v vt b bt) = LamT v (eraseTyM vt) (eraseTy b) (eraseTyM bt)
eraseTy (AppT f x t)     = AppT (eraseTy f) (eraseTy x) (eraseTyM t)
eraseTy (OLetT v vt b1 b2 t) = OLetT v (eraseTyM vt) (eraseTy b1) (eraseTy b2) (eraseTyM t)
eraseTy (RLetT v vt b1 b2 t) = RLetT v (eraseTyM vt) (eraseTy b1) (eraseTy b2) (eraseTyM t)

-- | 抹除 Maybe 包裹的类型注解中的 TyVar
eraseTyM :: Maybe (Typ TypeLitO TyVar) -> Maybe (Typ TypeLitO ())
eraseTyM = fmap eraseTyT

-- | 抹除 Typ 中的 TyVar（利用 Typ 的 Functor 实例映射 v）
eraseTyT :: Typ TypeLitO TyVar -> Typ TypeLitO ()
eraseTyT = fmap (const ())

-- | 解析源码并做类型推导，返回结果类型（成功）或抛出异常
parseAndCheck :: T.Text
    -> Either T.Text (Either (T.Forest TypeError) (TypedTerm (Typ TypeLitO ()) LitO TrmVar))
parseAndCheck src =
    case snd (simpleParser src parseExp) of
        Left doc  -> Left (T.pack (show doc))
        Right ast -> Right (runTC (eraseTy ast))

-- | 断言源码可以解析并通过类型检查，且结果类型匹配
shouldParseAndTypeCheck :: T.Text -> Typ TypeLitO () -> Expectation
shouldParseAndTypeCheck src expectedType =
    case parseAndCheck src of
        Left parseErr -> expectationFailure $
            "Parse error: " ++ T.unpack parseErr
        Right (Left tcErrs) -> expectationFailure $
            "Type error: " ++ show tcErrs
        Right (Right typedTerm) ->
            typOfTTerm typedTerm `shouldBe` expectedType

-- | 断言源码可以解析但类型检查失败
shouldParseAndFailTypeCheck :: T.Text -> Expectation
shouldParseAndFailTypeCheck src =
    case parseAndCheck src of
        Left parseErr -> expectationFailure $
            "Parse error (expected type error): " ++ T.unpack parseErr
        Right (Left _)  -> return ()
        Right (Right tm) -> expectationFailure $
            "Expected type error but got: " ++ T.unpack (tshow (typOfTTerm tm))

integrationSpec :: Spec
integrationSpec = describe "Parser + TypeChecker integration" $ do
    it "parses and type-checks integer literal" $
        shouldParseAndTypeCheck "42" intT
    it "parses and type-checks string literal" $
        shouldParseAndTypeCheck "\"hello\"" stringT
    it "parses and type-checks simple let binding" $
        shouldParseAndTypeCheck "let x = 1 in x" intT
    it "parses and type-checks nested let" $
        shouldParseAndTypeCheck "let x = 1 in let y = \"hello\" in x" intT
    it "parses and type-checks annotated expression" $
        shouldParseAndTypeCheck "1 :: Int" intT
    -- 类型错误测试
    it "rejects lambda without type annotation" $
        shouldParseAndFailTypeCheck "\\x -> x"
    it "rejects recursive let without type annotation" $
        shouldParseAndFailTypeCheck "let rec f = f in f"
    it "rejects application to non-function" $
        shouldParseAndFailTypeCheck "1 \"hello\""
