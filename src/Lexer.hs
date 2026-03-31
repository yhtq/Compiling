module Lexer where
import qualified Stream as S
import Effect
import Control.Applicative (Alternative(..), optional)
import GHC.Unicode (isOctDigit, isDigit, isHexDigit, isSpace)
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
import Data.HashSet (Set)
import qualified Data.HashSet as HS
import Data.HashMap (Map)
import Stream (TokensConstraints)
import Data.Coerce (coerce)

-- 从 1 开始计数
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
fromText t = TextStream (V.fromList (T.lines t), Position 1 1)

instance S.Stream TextStream where
    type Token TextStream = Char
    type Tokens TextStream = Text
    tokenLen = T.length
    uncons (TextStream (ts, pos)) = case V.uncons ts of
        Just (line, rest) -> case T.uncons line of
            Just (c, restLine) -> Just (c, TextStream (V.cons restLine rest, advanceColumn pos))
            Nothing -> Just ('\n', TextStream (rest, advanceLine pos))
        Nothing -> Nothing
    takeWhile_ p (TextStream (ts, pos)) = case V.uncons ts of
        Just (line, rest) -> let (t, r) = T.span p line in
            if T.null r then
                let (ts', pos') = S.takeWhile_ p (TextStream (rest, advanceLine pos)) in
                (T.cons '\n' t <> ts', pos')
            else
                (t, TextStream (V.cons r rest, advanceColumn pos))
        Nothing -> ("", TextStream (V.empty, pos))
    takeN_ n (TextStream (ts, pos)) = case V.uncons ts of
        Just (line, rest) -> let (t, r) = T.splitAt n line in
            if T.null r then
                let (ts', pos') = S.takeN_ (n - T.length line - 1) (TextStream (rest, advanceLine pos)) in
                (T.cons '\n' line <> ts', pos')
            else
                (t, TextStream (V.cons r rest, advanceColumns n pos))
        Nothing -> ("", TextStream (V.empty, pos))
    fromList = T.pack
    toList = T.unpack

newtype LexerError = LexerError Text deriving (Show, Eq, IsString, Semigroup, Monoid)

showPos :: Position -> Text
showPos pos = T.pack (show (column pos) ++ ":" ++ show (line pos))

type LexerEff es a = ParseEff TextStream LexerError es a
type SimpleLexer a = ParseEff TextStream LexerError '[CallingStackE, Hefty.Throw LexerError, Hefty.State [(Position, CallingStack)], ParserST TextStream] a

runCallingStackEWithLoc :: (ParserST TextStream :> es, HasParserCallingStack es, Hefty.State [(Position, CallingStack)] :> es) => ParseEff s err (CallingStackE : es) a -> ParseEff s err es a
runCallingStackEWithLoc = ParseEff . Hefty.interpret (\case
            Push st -> do
                stacks :: [(Position, CallingStack)] <- Hefty.get
                pos <- asEff getPos
                Hefty.put (stacks <> [(pos, st)])
                return ()
            Pop -> do
                stacks :: [(Position, CallingStack)] <- Hefty.get
                case stacks of
                    [] -> return "Error stack"
                    (h: t) -> do
                        Hefty.put t
                        return (snd h)
        ) . asEff

-- callingStackWithLoc :: (ParserST TextStream :> es) =>  Hefty.Eff (Hefty.State CallingStacks : es) a -> Hefty.Eff (Hefty.State (Position, CallingStack) :es) a

runThrowWithLoc :: (Hefty.FOEs es, ParserST TextStream :> es) => LexerEff (Hefty.Throw LexerError : es) a -> LexerEff es (Either LexerError a)
runThrowWithLoc p = do
    result <- runThrow p
    (TextStream (oriTs, _)) <- getParserState
    endPos <- getPos
    case result of
        Left e -> do
            showLine <- case oriTs V.!? (line endPos - 1) of
                Just line -> return line
                Nothing -> return "EOF"
            let message = T.intercalate "\n" [
                    "At " <> showPos endPos <> ": ",
                    "    " <> showLine,
                    "    " <> T.replicate (column endPos - 1) " " <> "^",
                    "   Lexer error: " <> coerce e
                    ]
            return $ Left (LexerError message)
        Right r -> return $ Right r

runSimpleLexer :: SimpleLexer a -> TextStream -> (TextStream, Either LexerError a)
runSimpleLexer peff initState = runPureParseEff $ runParserST (runThrowWithLoc peff) initState

colPos :: (ParserST TextStream :> es) => LexerEff es Position
colPos = do
    TextStream (_, pos) <- getParserState
    return pos

linePos :: (ParserST TextStream :> es) => LexerEff es Int
linePos = do    
    TextStream (_, pos) <- getParserState
    return $ line pos

getPos :: (ParserST TextStream :> es) => ParseEff TextStream err es Position
getPos = do
    TextStream (_, pos) <- getParserState
    return pos

char :: (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char) => Char -> ParseEff s err es Char
char c = satisfy_ (== c) 

newline :: (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char) => ParseEff s err es ()
newline = void $ satisfy_ isNewline 

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

parseInt :: forall s err es. (HasParserCallingStack es, ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char, ?base :: Int) => ParseEff s err es Int
parseInt = withCallingStack "parseInt" $ do
    sign <- optional (satisfy_ (\c -> c == '+' || c == '-'))
    ds <- digits
    case sign of
        Just '-' -> return $ - littleEnd ds
        _ -> return $ littleEnd ds

locatify :: (ParserST TextStream :> es) => LexerEff es a -> LexerEff es (Located a)
locatify p = do
    startPos <- getPos
    result <- p
    endPos <- getPos
    return $ Located (result, (startPos, endPos))

isNewline :: Char -> Bool
isNewline c = c == '\n' || c == '\r'

-- 用于提供 LL 文法指引
-- *我们假设出现在以下部分的标记不能出现在任何其他合法成分中*
-- 不允许递归块注释
data LexerConfig s = LexerConfig
    { 
        startOfLineComment :: S.Tokens s, -- 行首注释标记，如 "#"
        startOfBlockComment :: S.Tokens s, -- 块注释开始标记，如 "/*"
        endOfBlockComment :: S.Tokens s, -- 块注释结束标记，如 "*/"
        startOfNumericLiteral :: [S.Tokens s],
        startOfStringLiteral :: S.Tokens s,
        endOfStringLiteral :: S.Tokens s,
        startOfIdentifier :: Set (S.Token s), -- 标识符首字符集合，如字母和下划线
        spaceChars :: Set Char -- 需要跳过的空白字符集合
    } 


skipSpace :: (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char) => Set Char -> ParseEff s err es ()
skipSpace spaceChars = void $ takeWhileP (`HS.member` spaceChars)

skipLineComment :: (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char, TokensConstraints s) => S.Tokens s -> ParseEff s err es ()
skipLineComment start = 
    tokens start >> takeWhileP (not . isNewline) >> optional newline >> return ()

skipBlockComment :: (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char, TokensConstraints s) => S.Tokens s -> S.Tokens s -> S.Token s -> ParseEff s err es ()
skipBlockComment start end startCharOfEnd = do
    tokens start
    let aux = do
            void $ takeWhileP (/= startCharOfEnd)
            next <- try $ observing (tokens end)
            case next of
                Right _ -> return ()
                Left _ -> aux
    aux
parseStringLiteral :: (HasParserCallingStack es, ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char, TokensConstraints s, Monoid (S.Tokens s)) 
    => S.Tokens s -> S.Tokens s -> S.Token s -> ParseEff s err es (S.Tokens s)
parseStringLiteral start end startCharOfEnd = withCallingStack "parseStringLiteral" $ do
    tokens start
    let aux acc = do
            c <- takeWhileP (/= startCharOfEnd)
            next <- try $ observing (tokens end)
            case next of
                Right _ -> return acc
                Left _ -> do
                    aux (acc <> c)
    aux mempty

parseIdentifier :: forall s err es. (HasParserCallingStack es, ParseEffConstraints s err es, S.Stream s, Show (S.Token s), Monoid (S.Tokens s)) 
    => (S.Token s -> Bool) -> (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
parseIdentifier isStart isPart = withCallingStack "parseIdentifier" $ do
    firstChar <- satisfy_ isStart
    rest <- takeWhileP isPart
    return $ S.fromList @s [firstChar] <> rest

data Lexcial = LineComment 
    | BlockComment 
    | NumericLiteral Int
    | StringLiteral Text
    | Identifier Text
    | Space
    deriving (Show, Eq)

-- -- 这里的 int 是向前看的最大标记长度
-- genLLParsers :: LexerConfig -> (Int, Map Text (LexerEff es ()))
-- genLLParsers config = 
    

-- lexeme :: (ParseEffConstraints s err es, S.Stream s, S.Token s ~ Char) 
--         => LexerConfig -> LexerEff es a -> LexerEff es a
-- lexeme config p = do
