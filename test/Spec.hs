import Test.Hspec
import LexerSpec
import ParserSpec
import qualified Control.Monad.Hefty as Hefty
import qualified Control.Monad.Hefty.Except as Hefty

throwTest :: Hefty.Eff '[Hefty.Throw String, Hefty.Throw String] ()
throwTest = Hefty.raise (Hefty.throw "error1")

runThrow1 :: (Hefty.FOEs es) => Hefty.Eff (Hefty.Throw String : es) a -> Hefty.Eff es (Maybe String)
runThrow1 eff = do
    result <- Hefty.runThrow eff
    case result of
        Left e -> return (Just ("runThrow1: " ++ e))
        Right _ -> return Nothing

runThrow2 :: (Hefty.FOEs es) => Hefty.Eff (Hefty.Throw String : es) a -> Hefty.Eff es (Maybe String)
runThrow2 eff = do
    result <- Hefty.runThrow eff
    case result of
        Left e -> return (Just ("runThrow2: " ++ e))
        Right _ -> return Nothing

throwSpec :: Spec
throwSpec = it "Hefty.Throw should propagate through nested runThrow" $ do
    let result1 = Hefty.runPure (runThrow1 (runThrow2 throwTest))
    result1 `shouldBe` Just "runThrow1: error1"
    let result2 = Hefty.runPure (runThrow2 (runThrow1 throwTest))
    result2 `shouldBe` Just "runThrow2: error1"

main :: IO ()
main =
  hspec $ parallel $ do
  describe "Hefty.Throw" $ do
    throwSpec
  describe "Lexer" $ do
    littleEndSpec
    bigEndSpec
    charSpec
    digitsSpec
    lexerSpec
    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
  describe "Parser" $ do
    bindSpec
    expSpec
    typSpec
    annotatedSpec
  -- describe "read" $ do
  --   it "is inverse to show" $ property $
  --     \x -> (read . show) x `shouldBe` (x :: Int)
