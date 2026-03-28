module Lexer where
import qualified Stream as S
import Effect
import Control.Applicative (Alternative(..), optional)
import GHC.Unicode (isOctDigit, isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString)
import Data.Char (digitToInt)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Monad.Hefty ((:>))
import qualified Control.Monad.Hefty as Hefty
import qualified Control.Monad.Hefty.State as Hefty
import qualified Control.Monad.Hefty.Except as Hefty
import Control.Monad (void)

-- 从 1 开始计数
data Position = Position
    { line :: Int
    , column :: Int
    } deriving (Show, Eq)
advanceLine :: Position -> Position
advanceLine (Position l _) = Position (l + 1) 1
advanceColumn :: Position -> Position
advanceColumn (Position l c) = Position l (c + 1)
newtype Located a = Located (a, Position) deriving (Show, Eq, Functor)

-- 一个 Text 流状态，包括分行的原文本和当前(head of vector)所在位置
-- 换行符也会被输出
newtype TextStream = TextStream (Vector Text, Position) deriving (Show, Eq)
fromText :: Text -> TextStream
fromText t = TextStream (V.fromList (T.lines t), Position 1 1)

instance S.Stream TextStream where
    type Token TextStream = Char
    uncons (TextStream (ts, pos)) = case V.uncons ts of
        Just (line, rest) -> case T.uncons line of
            Just (c, restLine) -> Just (c, TextStream (V.cons restLine rest, advanceColumn pos))
            Nothing -> Just ('\n', TextStream (rest, advanceLine pos))
        Nothing -> Nothing
    

newtype LexerError = LexerError Text deriving (Show, Eq, IsString)
type LexerEff es a = ParseEff TextStream LexerError es a
type SimpleLexer a = ParseEff TextStream LexerError '[ParserST TextStream, Hefty.Throw LexerError] a

runSimpleLexer :: SimpleLexer a -> TextStream -> Either LexerError (TextStream, a)
runSimpleLexer peff initState = runPureParseEff $ runThrow $ runParserST peff initState

colPos :: (ParserST TextStream :> es) => LexerEff es Position
colPos = do
    TextStream (_, pos) <- getParserState
    return pos

linePos :: (ParserST TextStream :> es) => LexerEff es Int
linePos = do    
    TextStream (_, pos) <- getParserState
    return $ line pos

char :: (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char) => Char -> ParseEff s err es Char
char c = satisfy_ (== c) 

newline :: (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char) => ParseEff s err es ()
newline = void $ char '\n'

-- 根据 base 解析一位或多位整数，base 只能是 2, 8, 10, 16，否则永远失败
digits :: forall s err es. (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char, ?base :: Int) => ParseEff s err es [Char]
digits = do
    let _isDigit c = case ?base of
            2 -> c == '0' || c == '1'
            8 -> isOctDigit c
            10 -> isDigit c
            16 -> isHexDigit c
            _ -> False
    some (satisfy_ _isDigit)

-- 注意这里不处理负号
bigEnd :: (?base :: Int) => [Char] -> Int
bigEnd = foldr (\c acc -> acc * ?base + digitToInt c) 0

-- 注意这里不处理负号
littleEnd :: (?base :: Int) => [Char] -> Int
littleEnd = foldl (\acc c -> acc * ?base + digitToInt c) 0

parseInt :: forall s err es. (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char, ?base :: Int) => ParseEff s err es Int
parseInt = do
    sign <- optional (satisfy_ (\c -> c == '+' || c == '-'))
    ds <- digits
    case sign of
        Just '-' -> return $ - littleEnd ds
        _ -> return $ littleEnd ds

