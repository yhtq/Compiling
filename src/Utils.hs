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
-- 注意行列都是从 1 开始计数
data Position = Position
    { line :: Int
    , column :: Int
    } deriving (Show, Eq)
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

instance S.Stream TextStream where
    type Token TextStream = Char
    type Tokens TextStream = Text
    tokenLen = T.length
    uncons (TextStream (ts, pos)) = case V.uncons ts of
        Just (line, rest) -> case T.uncons line of
            Just (c, restLine) -> 
                let nextStream = if T.null restLine 
                    then 
                        (rest, advanceLine pos) 
                    else 
                        (V.cons restLine rest, advanceColumn pos) in
                Just (c, TextStream nextStream)
            Nothing -> S.uncons (TextStream (rest, advanceLine pos))
        Nothing -> Nothing
    takeWhile_ p (TextStream (ts, pos)) = case V.uncons ts of
        Just (line, rest) -> let (t, r) = T.span p line in
            if T.null r then
                let (ts', pos') = S.takeWhile_ p (TextStream (rest, advanceLine pos)) in
                (t <> ts', pos')
            else
                (t, TextStream (V.cons r rest, advanceColumns (T.length t) pos))
        Nothing -> ("", TextStream (V.empty, pos))
    takeN_ n (TextStream (ts, pos)) = case V.uncons ts of
        Just (line, rest) -> let (t, r) = T.splitAt n line in
            if T.null r then
                let (ts', pos') = S.takeN_ (n - T.length line - 1) (TextStream (rest, advanceLine pos)) in
                (line <> ts', pos')
            else
                (t, TextStream (V.cons r rest, advanceColumns n pos))
        Nothing -> ("", TextStream (V.empty, pos))
    fromList = T.pack
    toList = T.unpack

newtype LexerError = LexerError Text deriving (Show, Eq, Semigroup, Monoid)

instance IsText LexerError where
    fromText = LexerError

instance TShow LexerError where
    tshow (LexerError e) = e

showPos :: Position -> Text
showPos pos = T.pack (show (column pos) ++ ":" ++ show (line pos))

