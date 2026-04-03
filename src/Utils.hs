module Utils where
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Stream as S
import Control.Applicative (Alternative(..), optional)
import Effect
import qualified Control.Monad.Hefty as Hefty
import Control.Monad.Hefty ((:>))
import Exception
import Text (IsText(..), TShow(..))
import Prettyprinter (Pretty)
-- 注意行列都是从 1 开始计数
data Position = Position
    { line :: Int
    , column :: Int
    } deriving (Show, Eq)

instance TShow Position where
    tshow (Position l c) = T.pack (show l ++ ":" ++ show c)

advanceLine :: Position -> Position
advanceLine (Position l _) = Position (l + 1) 1
advanceColumn :: Position -> Position
advanceColumn (Position l c) = Position l (c + 1)
advanceColumns :: Int -> Position -> Position
advanceColumns n (Position l c) = Position l (c + n)

-- 一个带位置信息的值，包含值和它在文本中的起止位置
newtype Located a = Located (a, (Position, Position)) deriving (Show, Eq, Functor)

-- 一个 Text 流状态，包括分行的原文本和当前(head of vector)所在位置
-- 换行符也会被输出
newtype TextStream = TextStream (Vector Text, Position) deriving (Show, Eq)
fromText :: Text -> TextStream
fromText t = TextStream (V.fromList (map (<> "\n") (T.lines t)), Position 1 1)

instance TShow TextStream where
    tshow (TextStream (lines, pos)) = "TextStream at " <> tshow pos <> " with lines: " <> tshow (V.toList lines)

instance S.TokenClass TextStream where
    type Token TextStream = Char
    type Tokens TextStream = Text
    tokenLen = T.length
    fromList = T.pack
    toList = T.unpack

runPureStreamInState :: (ParserST TextStream :> es) => ParseEff TextStream err (S.Stream TextStream TextStream : es) a -> ParseEff TextStream err es a
runPureStreamInState = ParseEff . Hefty.interpret (
        \case
            S.TakeN n -> do
                ts <- asEff getParserState
                let go n (TextStream (ts, pos)) = if n <= 0 then return "" else do
                            case V.uncons ts of
                                Just (line, rest) -> let (t, r) = T.splitAt n line in
                                    if T.null r then 
                                        if n == T.length t then do 
                                            asEff $ putParserState (TextStream (rest, advanceLine pos))
                                            return t
                                        else do
                                            t' <- go (n - T.length t) (TextStream (rest, advanceLine pos))
                                            return (line <> t')
                                    else do                            
                                        asEff $ putParserState (TextStream (V.cons r rest, advanceColumns n pos))
                                        return t
                                Nothing -> do
                                    asEff $ putParserState (TextStream (V.empty, pos))
                                    return ""
                go n ts
            S.TakeWhile p -> do
                ts <- asEff getParserState
                let go (TextStream (ts, pos)) = do
                        case V.uncons ts of
                            Just (line, rest) -> let (t, r) = T.span p line in
                                if T.null r then do
                                    t' <- go (TextStream (rest, advanceLine pos))
                                    return (t <> t')
                                else do
                                    asEff $ putParserState (TextStream (V.cons r rest, advanceColumns (T.length t) pos))
                                    return t
                            Nothing -> do
                                asEff $ putParserState (TextStream (V.empty, pos))
                                return ""
                go ts
            S.Current -> asEff getParserState
            S.Revert buf -> asEff $ putParserState buf
        ) . asEff 

-- instance (Monad m) => S.StreamM m TextStream where
--     type Token TextStream = Char
--     type Tokens TextStream = Text
--     tokenLen = T.length
--     uncons (TextStream (ts, pos)) = case V.uncons ts of
--         Just (line, rest) -> case T.uncons line of
--             Just (c, restLine) -> 
--                 let nextStream = if T.null restLine 
--                     then 
--                         (rest, advanceLine pos) 
--                     else 
--                         (V.cons restLine rest, advanceColumn pos) in
--                 return $ Just (c, TextStream nextStream)
--             Nothing -> S.uncons (TextStream (rest, advanceLine pos))
--         Nothing -> return Nothing
--     takeWhile_ p (TextStream (ts, pos)) = case V.uncons ts of
--         Just (line, rest) -> let (t, r) = T.span p line in
--             if T.null r then do
--                 (ts', pos') <- S.takeWhile_ p (TextStream (rest, advanceLine pos)) 
--                 return (t <> ts', pos')
--             else
--                 return (t, TextStream (V.cons r rest, advanceColumns (T.length t) pos))
--         Nothing -> return ("", TextStream (V.empty, pos))
--     takeN_ n (TextStream (ts, pos)) = case V.uncons ts of
--         Just (line, rest) -> let (t, r) = T.splitAt n line in
--             if T.null r then do
--                 (ts', pos') <- S.takeN_ (n - T.length line - 1) (TextStream (rest, advanceLine pos)) 
--                 return (line <> ts', pos')
--             else
--                 return (t, TextStream (V.cons r rest, advanceColumns n pos))
--         Nothing -> return ("", TextStream (V.empty, pos))
--     fromList = T.pack
--     toList = T.unpack

newtype LexerError = LexerError Text deriving (Show, Eq, Semigroup, Monoid)

deriving instance Pretty LexerError

instance IsText LexerError where
    fromText = LexerError

instance TShow LexerError where
    tshow (LexerError e) = e

showPos :: Position -> Text
showPos pos = T.pack (show (column pos) ++ ":" ++ show (line pos))

