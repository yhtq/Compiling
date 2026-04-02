module Lexer where
import qualified Stream as S
import Effect
import Control.Applicative (Alternative(..), optional)
import GHC.Unicode (isOctDigit, isDigit, isHexDigit, isSpace)
import Data.Text (Text)
import Data.Char (digitToInt)
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

type LexerEff es a = ParseEff TextStream LexerError es a
type SimpleLexer a = ParseEff TextStream LexerError '[
        MultiThrow (LexerError, Position), Hefty.Ask Position, SourceViewer,  ParserST TextStream
    ] a


colPos :: (ParserST TextStream :> es) => LexerEff es Position
colPos = do
    TextStream (_, pos) <- getParserState
    return pos

linePos :: (ParserST TextStream :> es) => LexerEff es Int
linePos = do    
    TextStream (_, pos) <- getParserState
    return $ line pos


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
    asEff getPos
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


skipSpace :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char) => Set Char -> ParseEff s err es ()
skipSpace spaceChars = void $ takeWhileP (`HS.member` spaceChars)

skipLineComment :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char, TokensConstraints s) => S.Tokens s -> ParseEff s err es ()
skipLineComment start = 
    tokens start >> takeWhileP (not . isNewline) >> optional newline >> return ()

skipBlockComment :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char, TokensConstraints s) => S.Tokens s -> S.Tokens s -> S.Token s -> ParseEff s err es ()
skipBlockComment start end startCharOfEnd = do
    tokens start
    let aux = do
            void $ takeWhileP (/= startCharOfEnd)
            next <- try $ observing (tokens end)
            case next of
                Right _ -> return ()
                Left _ -> aux
    aux
parseStringLiteral :: (ParseEffFOEConstraints s st err es, S.Stream s, S.Token s ~ Char, TokensConstraints s, Monoid (S.Tokens s)) 
    => S.Tokens s -> S.Tokens s -> S.Token s -> ParseEff s err es (S.Tokens s)
parseStringLiteral start end startCharOfEnd = withInStack' "parseStringLiteral" $ do
    tokens start
    let aux acc = do
            c <- takeWhileP (/= startCharOfEnd)
            next <- try $ observing (tokens end)
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
    

-- lexeme :: (ParseEffConstraints s st err es, S.Stream s, S.Token s ~ Char) 
--         => LexerConfig -> LexerEff es a -> LexerEff es a
-- lexeme config p = do
