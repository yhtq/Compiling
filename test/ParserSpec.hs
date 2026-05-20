{-# LANGUAGE UndecidableInstances #-}
module ParserSpec where
import GHC.Stack (HasCallStack)
import Lexer
import Data.Text (Text)
import Test.Hspec (Expectation, shouldBe, Spec, it)
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

type SimpleParser a = ParseEff LexerStream ParserError (ParserES TextStream ++ LexerES) a
joinSnd :: (Monad m) => (a, m (m b)) -> (a, m b)
joinSnd (a, mb) = (a, join mb)

testParser :: Text -> SimpleParser a -> (a -> Expectation) -> Expectation
testParser src parser expected =
    let initStream = fromText src
        _lexer = lexer' @_ @TextStream @_ @LexerError defaultLexerConfig
        parserStream = runSimpleParser _lexer parser
        parserFinal = joinSnd $ runSimpleLexer parserStream initStream
    in
    case snd parserFinal of
        Left e -> Exception.throw $ AssertionFailed ((PP.renderString . PP.layoutPretty PP.defaultLayoutOptions) e)
        Right result -> expected result

testParserEq  :: (Eq a, Show a, HasCallStack) => Text -> SimpleParser a -> a -> Expectation
testParserEq src parser expected = testParser src parser (`shouldBe` expected)

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
    it "termVar" $ testParserEq "one" (parseVar @_ @_ @_ @_ @_ @Text) "one"
    it "bind" $ testParserSuccess "one = 1" parseBind

test :: Keywords -> Text
test = tshow
