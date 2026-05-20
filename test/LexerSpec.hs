module LexerSpec where
import Test.Hspec
import Test.QuickCheck
import Lexer
import qualified Data.Text as T
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP
import Utils
import Data.Text (Text)
import GHC.Exception (throw)
import Control.Exception (AssertionFailed(AssertionFailed))
import qualified Data.Vector as V
import Control.Applicative (Alternative(..), optional)
import Effect (try, getParserState, tokens, takeWhileP, ParseEff (..), satisfy_)


intToLittleEnd :: Int -> [Char]
intToLittleEnd = show

intToBigEnd :: Int -> [Char]
intToBigEnd = reverse . show

littleEndSpec :: Spec
littleEndSpec = it "littleEnd should be the inverse of intToLittleEnd" $
    property $ \x -> let ?base = 10 in
        if x >= 0 then
            littleEnd (intToLittleEnd x) == x
        else
            littleEnd (intToLittleEnd (-x)) == -x

bigEndSpec :: Spec
bigEndSpec = it "bigEnd should be the inverse of intToBigEnd" $
    property $ \x -> let ?base = 10 in
        if x >= 0 then
            bigEnd (intToBigEnd x) == x
        else
            bigEnd (intToBigEnd (-x)) == -x

charSpec :: Spec
charSpec = it "char should parse the expected character" $
    property $ conjoin $ map (
        \c ->
            let initStream = fromText (T.singleton c) in
            let parser = char c in
            let result = runSimpleLexer parser initStream in
            case result of
                (_, Left _) -> property False
                (TextStream (_, pos), Right parsedChar) ->
                    conjoin [
                        counterexample ("parsed char: " ++ show parsedChar ++ ", expected: " ++ show c) $ parsedChar == c,
                        counterexample ("position: " ++ show pos ++ ", expected: " ++ show (Position 1 2))
                            $ pos == Position 1 2
                    ]
        ) ['a', 'Z', '0', '9', '啊']

digitsSpec :: Spec
digitsSpec = it "digits should parse valid digits according to the base" $
    property $ conjoin $ map (
        \base ->
            let ?base = base in
            let toLittleEnd x = if x < 0
                then '-' : toLittleEnd (-x)
                else showIntAtBase base intToDigit x "" in
            \(x :: Int) ->
                let initStream = fromText (T.pack (toLittleEnd x)) in
                let parser = parseInt in
                let result = runSimpleLexer parser initStream in
                case result of
                    (_, Left e) -> counterexample ((PP.renderString . PP.layoutPretty PP.defaultLayoutOptions) (PP.vsep ["Lexer error: ", e])) $ property False
                    (TextStream (_, pos), Right parsedInt) ->
                        conjoin [
                            counterexample ("parsed int: " ++ show parsedInt ++ ", expected: " ++ show x) $ parsedInt == x,
                            counterexample ("position: " ++ show pos ++ ", expected: " ++ show (Position 1 (length (toLittleEnd x) + 1)))
                                $ pos == Position 1 (length (toLittleEnd x) + 1)
                            ]
    ) [2, 8, 10, 16]

testLexer :: (Show a, Eq a) => HasCallStack => Text -> SimpleLexer a -> a -> Expectation
testLexer input parser expected =
    let initStream = fromText input in
    case snd (runSimpleLexer parser initStream) of
        Left e -> throw $ AssertionFailed ((PP.renderString . PP.layoutPretty PP.defaultLayoutOptions) e)
        Right tokens -> tokens `shouldBe` expected

withPos :: SimpleLexer a -> SimpleLexer (a, V.Vector Text, Position)
withPos parser = do
    result <- parser
    TextStream (vt, pos) <- getParserState
    return (result, vt, pos)

manySpec1 :: Spec
manySpec1 = it "many' should parse zero or more occurrences" $
    testLexer "bbbbb" (withPos $ many (char 'a')) ([] :: [Char], V.fromList ["bbbbb\n"], Position 1 1)
manySpec2 :: Spec
manySpec2 = it "many' should parse multiple occurrences" $
    testLexer "aaaab" (withPos $ many (char 'a')) ("aaaa", V.fromList ["b\n"], Position 1 5)
    -- let parser = many (char 'a') in
    -- let initStream = fromText "aaab" in
    -- case runSimpleLexer parser initStream of
    --     (_, Left e) -> throw $ AssertionFailed ((PP.renderString . PP.layoutPretty PP.defaultLayoutOptions) e)
    --     (_, Right result) -> result `shouldBe` "aaa"


lexerSpec :: Spec
lexerSpec = describe "Lexer tests" $ do
    it "splitAt test" $ T.splitAt 1 "\nello" `shouldBe` ("\n", "ello")
    it "fromText test" $ fromText "aaa \n bbb" `shouldBe` TextStream (V.fromList ["aaa \n", " bbb\n"], Position 1 1)
    it "optional test" $ testLexer "bbbbb" (withPos $ optional $ try (char 'a')) (Nothing, V.fromList ["bbbbb\n"], Position 1 1)
    it "tokens test" $ testLexer "abcdb" (withPos $ tokens "abcd") ((), V.fromList ["b\n"], Position 1 5)
    it "tokens test 2" $ testLexer "abc" (withPos $ tokens "abc\n") ((), V.fromList [], Position 2 1)
    it "takeWhile test" $ testLexer "abcdb" (withPos $ takeWhileP (/= 'd')) ("abc", V.fromList ["db\n"], Position 1 4)
    manySpec1
    manySpec2
    it "simple line comment" $
        testLexer "-- this is a comment" (withPos (tokens "--" >> takeWhileP (not . isNewline) >> newline)) ((), V.fromList [], Position 2 1)
    it "should parse a line" $
        testLexer "-- this is a comment\n 100 _a啊121bc" (lexer defaultLexerConfig) [
            Located (LineComment, (Position 1 1, Position 1 21)),
            Located (Space, (Position 2 1, Position 2 1)),
            Located (NumericLiteral 100, (Position 2 2, Position 2 4)),
            Located (Space, (Position 2 5, Position 2 5)),
            Located (Identifier "_a啊121bc", (Position 2 6, Position 2 13)),
            Located (NewLine, (Position 2 14,Position 2 14))
            ]
