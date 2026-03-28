import Test.Hspec
import LexerSpec 

main :: IO ()
main = 
  hspec $ parallel $ do
  describe "Lexer" $ do
    littleEndSpec
    bigEndSpec
    charSpec
    digitsSpec
    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
  -- describe "read" $ do
  --   it "is inverse to show" $ property $
  --     \x -> (read . show) x `shouldBe` (x :: Int)