module LexerSpec where
import Test.Hspec
import Test.QuickCheck
import Lexer
import qualified Data.Text as T
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Coerce (coerce)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP
import Utils
import Data.Text (Text)
import Printer (Doc)
import GHC.Exception (throw)
import Control.Exception (AssertionFailed(AssertionFailed))
import qualified Data.Vector as V
import Lexer 
import Control.Applicative (Alternative(..))


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

manySpec1 :: Spec
manySpec1 = it "many' should parse zero or more occurrences" $
    testLexer "" (many (char 'a')) ([] :: [Char])
manySpec2 :: Spec
manySpec2 = it "many' should parse multiple occurrences" $
    testLexer "aaaab" (many (char 'a')) "aaaa"
    -- let parser = many (char 'a') in
    -- let initStream = fromText "aaab" in
    -- case runSimpleLexer parser initStream of
    --     (_, Left e) -> throw $ AssertionFailed ((PP.renderString . PP.layoutPretty PP.defaultLayoutOptions) e)
    --     (_, Right result) -> result `shouldBe` "aaa"


lexerSpec :: Spec
lexerSpec = describe "Lexer tests" $ do
    it "fromText test" $ fromText "aaa \n bbb" `shouldBe` TextStream (V.fromList ["aaa \n", " bbb\n"], Position 1 1)
    manySpec1
    manySpec2
    -- it "should parse a simple line comment" $
    --     testLexer "-- this is a comment\n 100 _a啊121bc" [
    --         Located (LineComment, (Position 1 1, Position 1 21)),
    --         Located (Space, (Position 2 1, Position 2 1)),
    --         Located (NumericLiteral 100, (Position 2 2, Position 2 4)),
    --         Located (Space, (Position 2 5, Position 2 5)),
    --         Located (Identifier "_a啊121bc", (Position 2 6, Position 2 13)),
    --         Located (NewLine, (Position 2 14,Position 2 14))
    --         ]