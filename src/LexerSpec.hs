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
                let initStream = fromText ("啊" <> T.pack (toLittleEnd x)) in
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