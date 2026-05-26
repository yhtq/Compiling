{-# LANGUAGE UndecidableInstances #-}
module ParserSpec where
import GHC.Stack (HasCallStack)
import Lexer
import Data.Text (Text)
import Test.Hspec (Expectation, shouldBe, Spec, it, shouldNotBe)
import Effect
import Utils
import Parser
import Control.Monad.Hefty (type (++))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP
import Control.Monad (join)
import Control.Exception.Base (AssertionFailed(..))
import qualified Control.Exception.Base as Exception
import Text (TShow(..), tshow)
import qualified Stream as S
import Data.Foldable (Foldable(toList))
import Data.Maybe (fromJust)
import Printer (Doc)
import Ast

type SimpleParser a = ParseEff LexerStream ParserError (ParserES TextStream ++ LexerES) a
joinSnd :: (Monad m) => (a, m (m b)) -> (a, m b)
joinSnd (a, mb) = (a, join mb)

simpleParser :: Text -> SimpleParser a -> (TextStream, Either Doc a)
simpleParser src parser =
    let initStream = fromText src
        _lexer = lexer' @_ @TextStream @_ @LexerError defaultLexerConfig
        parserStream = runSimpleParser _lexer parser
    in joinSnd $ runSimpleLexer parserStream initStream

testParser :: Text -> SimpleParser a -> (a -> Expectation) -> Expectation
testParser src parser expected =
    case snd (simpleParser src parser) of
        Left e -> Exception.throw $ AssertionFailed ((PP.renderString . PP.layoutPretty PP.defaultLayoutOptions) e)
        Right result -> expected result

testParserEq  :: (Eq a, Show a, HasCallStack) => Text -> SimpleParser a -> a -> Expectation
testParserEq src parser expected = testParser src parser (`shouldBe` expected)

testParserNEq :: (Eq a, Show a, HasCallStack) => Text -> SimpleParser a -> a -> Expectation
testParserNEq src parser unexpected = testParser src parser (`shouldNotBe` unexpected)

testParserSuccess :: Text -> SimpleParser a -> Expectation
testParserSuccess src parser = testParser src parser (const $ return ())

parseLexcial :: SimpleParser [Located LexcialR']
parseLexcial = ParseEff $ toList <$> S.takeWhile (not . isEOFR)

bindSpec :: Spec
bindSpec = do
    it "token stream" $ testParserEq "one = 1" parseLexcial [
        Located (IdentifierR "one",(Position {line = 1, column = 1},Position {line = 1, column = 3})),
        Located (KeywordR Equal ,(Position {line = 1, column = 5},Position {line = 1, column = 5})),
        Located (NumericLiteralR 1,(Position {line = 1, column = 7},Position {line = 1, column = 7}))]
    it "complex token stream" $ testParserEq "one = 1" (
            ParseEff $ sequence [
                fromJust <$> S.peek,
                fromJust <$> S.head,
                fromJust <$> S.head,
                fromJust <$> S.head
            ]
        ) [
            Located (IdentifierR "one",(Position {line = 1, column = 1},Position {line = 1, column = 3})),
            Located (IdentifierR "one",(Position {line = 1, column = 1},Position {line = 1, column = 3})),
            Located (KeywordR Equal ,(Position {line = 1, column = 5},Position {line = 1, column = 5})),
            Located (NumericLiteralR 1,(Position {line = 1, column = 7},Position {line = 1, column = 7}))]
    it "bind" $ testParserSuccess "one = 1" parseBind
    it "bind rec" $ testParserSuccess "rec one = 1" parseBind
expSpec :: Spec
expSpec = do
    it "number literal" $ testParserSuccess "1" parseExp
    it "identifier" $ testParserSuccess "one" parseExp
    it "parenthesized expression" $ testParserSuccess "(1)" parseExp
    it "function application" $ testParserSuccess "f x" parseExp
    it "function application with multiple arguments" $ testParserSuccess "f x y" parseExp
    it "abstraction" $ testParserSuccess "\\x -> x" parseExp
    it "abstraction apply" $ testParserSuccess "(\\x -> x) 1" parseExp

typSpec :: Spec
typSpec = do
    it "int type" $ testParserSuccess "Int" parseTy
    it "function type" $ testParserSuccess "Int -> Int" parseTy
    it "function type2" $ testParserEq "Int -> Int -> Int" parseTy (
            TFun (TLit TInt) (TFun (TLit TInt) (TLit TInt))
        )
    it "function type left" $ testParserEq "(Int -> Int) -> Int" parseTy (
            TFun (TFun (TLit TInt) (TLit TInt)) (TLit TInt)
        )


annotatedSpec :: Spec
annotatedSpec = do
    it "annotated expression" $ testParserSuccess "x :: Int" parseExp
    it "complex annotated expression" $ testParserNEq "((add :: Int -> Int) 1 2) :: Int" parseExp (
        assumeRight $ snd (simpleParser "(add 1 2)" parseExp)
        )

letSpec :: Spec
letSpec = do
    it "let expression" $ testParserSuccess "let x = 1 in x" parseExp
    it "let rec expression" $ testParserSuccess "let rec f = \\x -> ite (Eq x 0) 1 (mul x (f (sub x 1))) in f 5" parseExp
    it "nested let" $ testParserEq "let rec x = 1 in let y = 2 in add x y" parseExp (
        RLetT (TrmVar "x") Nothing (LitT (LInt 1) Nothing) (
            OLetT (TrmVar "y") Nothing (LitT (LInt 2) Nothing) (
                AppT (VarT (TrmVar "add") Nothing) (AppT (VarT (TrmVar "x") Nothing) (VarT (TrmVar "y") Nothing) Nothing) Nothing
            ) Nothing)
        Nothing
        )

test :: Keywords -> Text
test = tshow
