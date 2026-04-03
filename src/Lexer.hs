{-# LANGUAGE UndecidableInstances #-}
module Lexer where
import qualified Stream as S
import Effect
import Control.Applicative (Alternative(..), optional)
import GHC.Unicode (isOctDigit, isDigit, isHexDigit, isSpace, isAlpha, isAlphaNum)
import Control.Monad.Hefty ((:>))
import qualified Control.Monad.Hefty as Hefty
import Control.Monad (void)
import Data.HashSet (Set)
import qualified Data.HashSet as HS
import Exception (MultiThrow)
import Printer
import Utils
import qualified Control.Monad.Hefty.Reader as Hefty
import Text
import Data.String (IsString)
import Data.Char (digitToInt)
import Data.Text (unpack)
import qualified Data.Text as T

type LexerEff es a = ParseEff TextStream LexerError es a

-- effect stack needed for lexing
type LexerES = '[
        MultiThrow (LexerError, Position), Hefty.Ask Position, SourceViewer,  ParserST TextStream
    ]
type SimpleLexer a = ParseEff TextStream LexerError LexerES a

getPos :: (Hefty.Ask Position :> es) => ParseEff s err es Position
getPos = ParseEff Hefty.ask

colPos :: (Hefty.Ask Position :> es) => ParseEff s err es Position
colPos = fmap (\(Position _ c) -> Position 1 c) getPos

linePos :: (Hefty.Ask Position :> es) => ParseEff s err es Position
linePos = fmap (\(Position l _) -> Position l 1) getPos


runStatefulThrowWithLoc :: (Hefty.FOEs es, HasSourceViewer es) => LexerEff (MultiThrow (LexerError, Position) : es) a -> LexerEff es (Either Doc a)
runStatefulThrowWithLoc p = do
    result <- runThrow p
    case result of
        Left errorTree -> do
            let errorTree1 = fmap (fmap (\(LexerError e, pos) -> (pretty e, (pos, pos)))) errorTree
            msg <- printErrorForestE errorTree1
            return $ Left msg
        Right r -> return $ Right r

runPositionAsk :: (ParserST TextStream :> es) => LexerEff (Hefty.Ask Position : es) a -> LexerEff es a
runPositionAsk = ParseEff . Hefty.interpret (\Hefty.Ask -> do
    TextStream (_, pos) <- asEff getParserState
    return pos
    ) . asEff

runSimpleLexer :: SimpleLexer a -> TextStream -> (TextStream, Either Doc a)
runSimpleLexer peff initState@(TextStream (sources, _)) = 
    runPureParseEff $ 
    runParserST (
        ((ParseEff . Hefty.runAsk sources . asEff) . runPositionAsk . runStatefulThrowWithLoc) peff
    ) initState


char :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char) => Char -> ParseEff s err es Char
char c = satisfy_ (== c) 

newline :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char) => ParseEff s err es ()
newline = void $ satisfy_ isNewline 

-- 根据 base 解析一位或多位整数，base 只能是 2, 8, 10, 16，否则永远失败
digits :: forall s st err es. (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char, ?base :: Int) => ParseEff s err es [Char]
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

parseInt :: forall s st err es. (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char, ?base :: Int) => ParseEff s err es Int
parseInt = withInStack' "parseInt" $ do
    sign <- optional (satisfy_ (\c -> c == '+' || c == '-'))
    ds <- digits
    case sign of
        Just '-' -> return $ - littleEnd ds
        _ -> return $ littleEnd ds

-- 倒退一个位置
-- 如果在行首，则退到上一行的末尾
-- 不幸的是，这里需要知道上一行的末尾位置，因此必须访问 SourceViewer 来实现
retreatPos :: (HasSourceViewer es) => Position -> ParseEff s err es Position
retreatPos (Position l 1) = do
    lastLine <- getSourceLine (l - 1)
    case lastLine of
        Nothing -> return $ Position (l - 1) 0
        Just lastEndPos ->
            return $ Position (l - 1) (T.length lastEndPos)
retreatPos (Position l c) = return $ Position l (c - 1)


locatify :: (Hefty.Ask Position :> es, HasSourceViewer es) => ParseEff s err es a -> ParseEff s err es (Located a)
locatify p = do
    startPos <- getPos
    result <- p
    endPos <- getPos >>= retreatPos
    return $ Located (result, (startPos,  endPos))

isNewline :: Char -> Bool
isNewline c = c == '\n' || c == '\r'

-- 用于提供 LL 文法指引
-- *我们假设出现在以下部分的标记不能出现在任何其他合法成分中*
-- 不允许递归块注释
type NonEmptyTokens s = (S.Token s, S.Tokens s)
data LexerConfig s = LexerConfig
    { 
        startOfLineComment :: S.Tokens s, -- 行首注释标记，如 "#"
        startOfBlockComment :: S.Tokens s, -- 块注释开始标记，如 "/*"
        endOfBlockComment :: NonEmptyTokens s, -- 块注释结束标记，如 "*/"
        startOfStringLiteral :: S.Tokens s,
        endOfStringLiteral :: NonEmptyTokens s,
        startOfIdentifier :: S.Token s -> Bool, -- 标识符首字符集合，如字母和下划线
        partOfIdentifier :: S.Token s -> Bool, -- 标识符非首字符集合，如字母、数字和下划线
        spaceChars :: S.Token s -> Bool -- 需要跳过的空白字符集合
    } 


skipSpace :: (ParseEffFOEConstraints s st err es, S.Stream s, TShow (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es ()
skipSpace spaceChars = withInStack' "skipSpace" $ void $ takeWhileP1 spaceChars

skipLineComment :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char, TokensConstraints s) => S.Tokens s -> ParseEff s err es ()
skipLineComment start = withInStack' "skipLineComment" $
    tokens start >> takeWhileP (not . isNewline) >> newline >> return ()

skipBlockComment :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char, TokensConstraints s) => S.Tokens s -> NonEmptyTokens s -> ParseEff s err es ()
skipBlockComment start (startCharOfEnd, restEnd) = withInStack' "skipBlockComment" $  do
    tokens start
    let aux = do
            void $ takeWhileP (/= startCharOfEnd)
            _ <- char startCharOfEnd
            next <- try $ observing (tokens restEnd)
            case next of
                Right _ -> return ()
                Left _ -> aux
    aux
parseStringLiteral :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char, TokensConstraints s, Monoid (S.Tokens s)) 
    => S.Tokens s -> NonEmptyTokens s -> ParseEff s err es (S.Tokens s)
parseStringLiteral start (startCharOfEnd, restEnd) = withInStack' "parseStringLiteral" $ do
    tokens start
    let aux acc = do
            c <- takeWhileP (/= startCharOfEnd)
            _ <- char startCharOfEnd
            next <- try $ observing (tokens restEnd)
            case next of
                Right _ -> return acc
                Left _ -> do
                    aux (acc <> c)
    aux mempty

parseIdentifier :: forall s st err es. (ParseEffFOEConstraints s st err es, S.Stream s, TShow (S.Token s), Monoid (S.Tokens s)) 
    => (S.Token s -> Bool) -> (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
parseIdentifier isStart isPart = withInStack' "parseIdentifier" $ do
    firstChar <- satisfy_ isStart
    rest <- takeWhileP isPart
    return $ S.fromList @s [firstChar] <> rest

-- 注意 LineComment 会消耗一个换行符
data Lexcial s = LineComment 
    | BlockComment 
    | NumericLiteral Int
    | StringLiteral (S.Tokens s)
    | Identifier (S.Tokens s)
    | Space
    | NewLine

instance (Eq (S.Tokens s)) => Eq (Lexcial s) where
    LineComment == LineComment = True
    BlockComment == BlockComment = True
    Space == Space = True
    NewLine == NewLine = True
    NumericLiteral n1 == NumericLiteral n2 = n1 == n2
    StringLiteral s1 == StringLiteral s2 = s1 == s2
    Identifier i1 == Identifier i2 = i1 == i2
    _ == _ = False
    

instance (TShow (S.Tokens s)) => TShow (Lexcial s) where
    tshow LineComment = "..."
    tshow BlockComment = "{...}"
    tshow (NumericLiteral n) = tshow n
    tshow (StringLiteral s) = "\"" <> tshow s <> "\""
    tshow (Identifier s) = "ID(" <> tshow s <> ")"
    tshow Space = "Space"
    tshow NewLine = "NewLine"

instance (TShow (S.Tokens s)) => Show (Lexcial s) where
    show = unpack . tshow

lexer :: (ParseEffFOEConstraints s st err es, S.Stream s, TokensConstraints s, S.Token s ~ Char, Monoid (S.Tokens s), 
        Hefty.Ask Position :> es, HasSourceViewer es) => 
    LexerConfig s -> ParseEff s err es [Located (Lexcial s)]
lexer config = 
    let ?base = 10 in
    some $ locatify $ anyOf $ map try  [
        skipSpace (spaceChars config) >> return Space,
        newline >> return NewLine,
        skipLineComment (startOfLineComment config) >> return LineComment,
        skipBlockComment (startOfBlockComment config) (endOfBlockComment config) >> return BlockComment,
        NumericLiteral <$> parseInt,
        StringLiteral <$> parseStringLiteral (startOfStringLiteral config) (endOfStringLiteral config),
        Identifier <$> parseIdentifier (startOfIdentifier config) (partOfIdentifier config)
    ]

defaultLexerConfig :: (S.Token s ~ Char, IsString (S.Tokens s)) => LexerConfig s
defaultLexerConfig = LexerConfig
    { startOfLineComment = "--"
    , startOfBlockComment = "-*"
    , endOfBlockComment = ('*', "-")
    , startOfStringLiteral = "\""
    , endOfStringLiteral = ('\"', "")
    , startOfIdentifier = \c -> isAlpha c || c == '_'
    , partOfIdentifier = \c -> isAlphaNum c || c == '_'
    , spaceChars = \x -> isSpace x && not (isNewline x)
    }

-- -- 这里的 int 是向前看的最大标记长度
-- genLLParsers :: LexerConfig -> (Int, Map Text (LexerEff es ()))
-- genLLParsers config = 
    

-- lexeme :: (ParseEffConstraints s st err es, S.Stream s, S.Token s ~ Char) 
--         => LexerConfig -> LexerEff es a -> LexerEff es a
-- lexeme config p = do
