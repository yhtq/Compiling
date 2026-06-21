{-# LANGUAGE UndecidableInstances #-}
module Lexer where
import qualified Stream as S
import Effect
import Control.Applicative (Alternative(..), optional)
import GHC.Unicode (isOctDigit, isDigit, isHexDigit, isSpace, isAlpha, isAlphaNum)
import Control.Monad.Hefty ((:>))
import qualified Control.Monad.Hefty as Hefty
import Control.Monad (void)
import Exception (MultiThrow)
import Printer
import Utils (LexerError, TextStream(..), Position(..), Located(..), FatalError, runThrowFatalAsFail)
import qualified Control.Monad.Hefty.Reader as Hefty
import Text
import Data.String (IsString)
import Data.Char (digitToInt)
import Data.Text (unpack)
import qualified Data.Text as T
import Prettyprinter (Pretty)
import qualified Utils as S

type LexerEff es a = ParseEff TextStream LexerError es a

-- effect stack needed for lexing
type LexerES = '[
       S.Stream TextStream TextStream, MultiThrow (LexerError, Position), Hefty.Ask Position, SourceViewer, ParserST TextStream, Hefty.Throw FatalError
    ]
type SimpleLexer a = ParseEff TextStream LexerError LexerES a

getPos :: (Hefty.Ask Position :> es) => ParseEff s err es Position
getPos = ParseEff Hefty.ask

colPos :: (Hefty.Ask Position :> es) => ParseEff s err es Position
colPos = fmap (\(Position _ c) -> Position 1 c) getPos

linePos :: (Hefty.Ask Position :> es) => ParseEff s err es Position
linePos = fmap (\(Position l _) -> Position l 1) getPos


runStatefulThrowWithLoc :: (Hefty.FOEs es, HasSourceViewer es, Pretty e) => LexerEff (MultiThrow (e, Position) : es) a -> LexerEff es (Either Doc a)
runStatefulThrowWithLoc p = do
    result <- runThrow p
    case result of
        Left errorTree -> do
            let errorTree1 = fmap (fmap (\(e, pos) -> (pretty e, (pos, pos)))) errorTree
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
    (ParseEff . runThrowFatalAsFail . asEff) $
    runParserST (
        (
            (ParseEff . Hefty.runAsk sources . asEff) .
            runPositionAsk . runStatefulThrowWithLoc . S.runPureStreamInState
        ) peff
    ) initState


char :: (ParseEffFOEWithTokens snap s st err es, S.Token s ~ Char) => Char -> ParseEff s err es Char
char c = satisfy_ (== c)

newline :: (ParseEffFOEWithTokens snap s st err es, S.Token s ~ Char) => ParseEff s err es ()
newline = void $ satisfy_ isNewline

-- 根据 base 解析一位或多位整数，base 只能是 2, 8, 10, 16，否则永远失败
digits :: forall snap s st err es. (ParseEffFOEWithTokens snap s st err es, S.Token s ~ Char, Monoid (S.Tokens s), ?base :: Int) => ParseEff s err es [Char]
digits = do
    let _isDigit c = case ?base of
            2 -> c == '0' || c == '1'
            8 -> isOctDigit c
            10 -> isDigit c
            16 -> isHexDigit c
            _ -> False
    takeWhileP1'  _isDigit

-- 注意这里不处理负号
bigEnd :: (?base :: Int) => [Char] -> Int
bigEnd = foldr (\c acc -> acc * ?base + digitToInt c) 0

-- 注意这里不处理负号
littleEnd :: (?base :: Int) => [Char] -> Int
littleEnd = foldl (\acc c -> acc * ?base + digitToInt c) 0

parseInt :: forall snap s st err es. (ParseEffFOEWithTokens snap s st err es, S.Token s ~ Char, Monoid (S.Tokens s), ?base :: Int) => ParseEff s err es Int
parseInt = withInStack' "parseInt" $ do
    sign <- optional $ try (satisfy_ (\c -> c == '+' || c == '-'))
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
        keywords :: [S.Tokens s], -- 关键字集合
        startOfIdentifier :: S.Token s -> Bool, -- 标识符首字符集合，如字母和下划线
        partOfIdentifier :: S.Token s -> Bool, -- 标识符非首字符集合，如字母、数字和下划线
        spaceChars :: S.Token s -> Bool -- 需要跳过的空白字符集合
    }


skipSpace :: (ParseEffFOEWithTokens snap s st err es, TShow (S.Token s), Monoid (S.Tokens s)) => (S.Token s -> Bool) -> ParseEff s err es ()
skipSpace spaceChars = withInStack' "skipSpace" $ void $ takeWhileP1 spaceChars

skipLineComment :: (ParseEffFOEWithTokens snap s st err es, S.Token s ~ Char, TokensConstraints s) => S.Tokens s -> ParseEff s err es ()
skipLineComment start = withInStack' "skipLineComment" $
    tokens start >> takeWhileP (not . isNewline) >> newline >> return ()

skipBlockComment :: (ParseEffFOEWithTokens snap s st err es, S.Token s ~ Char, TokensConstraints s) => S.Tokens s -> NonEmptyTokens s -> ParseEff s err es ()
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
parseStringLiteral :: (ParseEffFOEWithTokens snap s st err es, S.Token s ~ Char, TokensConstraints s, Monoid (S.Tokens s))
    => S.Tokens s -> NonEmptyTokens s -> ParseEff s err es (S.Tokens s)
parseStringLiteral start (startCharOfEnd, restEnd) = withInStack' "parseStringLiteral" $ do
    tokens start
    let aux acc = do
            c <- takeWhileP (/= startCharOfEnd)
            _ <- char startCharOfEnd
            next <- try $ observing (tokens restEnd)
            case next of
                Right _ -> return (acc <> c)
                Left _ -> do
                    aux (acc <> c)
    aux mempty

parseIdentifier :: forall snap s st err es. (ParseEffFOEWithTokens snap s st err es, TShow (S.Token s), Monoid (S.Tokens s))
    => (S.Token s -> Bool) -> (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
parseIdentifier isStart isPart = withInStack' "parseIdentifier" $ do
    firstChar <- satisfy_ isStart
    rest <- takeWhileP isPart
    return $ S.fromList @s [firstChar] <> rest

-- TODO 通过提取前缀提高性能
parseKeyword :: (ParseEffFOEWithTokens snap s st err es, TShow (S.Tokens s), Eq (S.Tokens s)) => [S.Tokens s] -> ParseEff s err es (S.Tokens s)
parseKeyword = withInStack' "parseKeyword" . anyOf . map (\k -> (try $ tokens k *> return k))

-- 注意 LineComment 会消耗一个换行符
data Lexcial s = LineComment
    | BlockComment
    | NumericLiteral Int
    | StringLiteral (S.Tokens s)
    | Identifier (S.Tokens s)
    | Keyword (S.Tokens s)
    | Space
    | NewLine
    | EOF

-- 经过预处理的词法单元，已经区分了标识符和关键字
data LexcialR s k = NumericLiteralR Int
    | StringLiteralR (S.Tokens s)
    | IdentifierR (S.Tokens s)
    | KeywordR k
    | EOFR

collectJust :: [Maybe a] -> [a]
collectJust [] = []
collectJust (Just x : xs) = x : collectJust xs
collectJust (Nothing : xs) = collectJust xs

refineLexcial :: (S.Tokens s -> Maybe k) -> Lexcial s -> Maybe (LexcialR s k)
refineLexcial _ (NumericLiteral n) = Just $ NumericLiteralR n
refineLexcial _ (StringLiteral s) = Just $ StringLiteralR s
refineLexcial _ EOF = Just EOFR
refineLexcial toKey (Keyword s) = Just $ case toKey s of
    Just k -> KeywordR k
    Nothing -> IdentifierR s
refineLexcial toKey (Identifier s) = Just $ case toKey s of
    Just k -> KeywordR k
    Nothing -> IdentifierR s
refineLexcial _ _ = Nothing

refineLexcials :: (S.Tokens s -> Maybe k) -> [Lexcial s] -> [LexcialR s k]
refineLexcials toKey = collectJust . map (refineLexcial toKey)

instance (Eq (S.Tokens s)) => Eq (Lexcial s) where
    LineComment == LineComment = True
    BlockComment == BlockComment = True
    Space == Space = True
    NewLine == NewLine = True
    NumericLiteral n1 == NumericLiteral n2 = n1 == n2
    StringLiteral s1 == StringLiteral s2 = s1 == s2
    Identifier i1 == Identifier i2 = i1 == i2
    Keyword k1 == Keyword k2 = k1 == k2
    _ == _ = False

deriving instance (Eq (S.Tokens s), Eq k) => Eq (LexcialR s k)

instance (TShow (S.Tokens s)) => TShow (Lexcial s) where
    tshow LineComment = "..."
    tshow BlockComment = "{...}"
    tshow (NumericLiteral n) = tshow n
    tshow (StringLiteral s) = "\"" <> tshow s <> "\""
    tshow (Identifier s) = "ID(" <> tshow s <> ")"
    tshow Space = "Space"
    tshow NewLine = "NewLine"
    tshow (Keyword k) = "KW(" <> tshow k <> ")"
    tshow EOF = ""

instance (TShow (S.Tokens s), TShow k) => TShow (LexcialR s k) where
    tshow (NumericLiteralR n) = tshow n
    tshow (StringLiteralR s) = "\"" <> tshow s <> "\""
    tshow (IdentifierR s) = "ID(" <> tshow s <> ")"
    tshow (KeywordR k) = "KW(" <> tshow k <> ")"
    tshow EOFR = ""

instance (TShow (S.Tokens s)) => Show (Lexcial s) where
    show = unpack . tshow

instance (TShow (S.Tokens s), TShow k) => Show (LexcialR s k) where
    show = unpack . tshow

lexer' :: (ParseEffFOEWithTokens snap s st err es, TokensConstraints s, S.Token s ~ Char, Monoid (S.Tokens s),
        Hefty.Ask Position :> es, HasSourceViewer es) =>
    LexerConfig s -> ParseEff s err es (Located (Lexcial s))
lexer' config =
    let ?base = 10 in
    locatify $ anyOf (map try [
        skipSpace (spaceChars config) >> return Space,
        newline >> return NewLine,
        skipLineComment (startOfLineComment config) >> return LineComment,
        skipBlockComment (startOfBlockComment config) (endOfBlockComment config) >> return BlockComment,
        NumericLiteral <$> parseInt,
        StringLiteral <$> parseStringLiteral (startOfStringLiteral config) (endOfStringLiteral config),
        -- identifier 优先于 keyword，避免 keyword 被作为 identifier 前缀误匹配
        -- 例如 intToStr 不会被误拆为 KW(in) + ID(tToStr)
        -- 标识符类 keyword (let/in/rec) 由 refineLexcial 在词法分析后转换
        Identifier <$> parseIdentifier (startOfIdentifier config) (partOfIdentifier config),
        Keyword <$> parseKeyword (keywords config),
        eof >> return EOF
    ])

isEOF :: Located (Lexcial s) -> Bool
isEOF (Located (EOF, _)) = True
isEOF _ = False

isEOFR :: Located (LexcialR s k) -> Bool
isEOFR (Located (EOFR, _)) = True
isEOFR _ = False

lexer :: (ParseEffFOEWithTokens snap s st err es, TokensConstraints s, S.Token s ~ Char, Monoid (S.Tokens s),
        Hefty.Ask Position :> es, HasSourceViewer es) =>
    LexerConfig s -> ParseEff s err es [Located (Lexcial s)]
lexer config = do
    next <- lexer' config
    case next of
        Located (EOF, _) -> return []
        _ -> do
            rest <- lexer config
            return (next : rest)

defaultLexerConfig :: (S.Token s ~ Char, IsString (S.Tokens s)) => LexerConfig s
defaultLexerConfig = LexerConfig
    { startOfLineComment = "--"
    , startOfBlockComment = "-*"
    , endOfBlockComment = ('*', "-")
    , startOfStringLiteral = "\""
    , endOfStringLiteral = ('\"', "")
    , startOfIdentifier = \c -> isAlpha c || c == '_'
    , partOfIdentifier = \c -> isAlphaNum c || c == '_'
    -- , startOfIdentifier = \c -> not (c == '-' || c == '"' || isSpace c)
    -- , partOfIdentifier = \c -> not (c == '"' || isSpace c)
    , keywords = ["let", "in", "rec", "=", "\\", "->", "::", "(", ")"]
    , spaceChars = \x -> isSpace x && not (isNewline x)
    }

-- -- 这里的 int 是向前看的最大标记长度
-- genLLParsers :: LexerConfig -> (Int, Map Text (LexerEff es ()))
-- genLLParsers config =


-- lexeme :: (ParseEffConstraints s st err es, S.Stream s, S.Token s ~ Char)
--         => LexerConfig -> LexerEff es a -> LexerEff es a
-- lexeme config p = do
