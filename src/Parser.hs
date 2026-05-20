module Parser where
import Ast
import Lexer
import Effect
import Text
import qualified Data.Text as T
import qualified Stream as S
import qualified Control.Monad.Hefty as Hefty
import Control.Monad.Hefty.Input (Input)
import qualified Control.Monad.Hefty.Input as Hefty
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (Foldable(toList))
import Control.Monad.Hefty ((:>), type (++))
import Control.Monad.Hefty.State (State)
import qualified Control.Monad.Hefty.State as Hefty
import qualified Control.Monad.Hefty.Reader as Hefty
import Data.Functor ((<&>))
import Control.Applicative ((<|>), optional)
import Control.Monad (void)
import Exception (MultiThrow)
import Utils (Located (Located), unlocated, Into (into), dummyPos)
import Printer (HasSourceViewer, Doc, printErrorForestE)
import Prettyprinter (pretty, Pretty)


seqLast :: Seq a -> Maybe a
seqLast seq = case Seq.viewr seq of
    Seq.EmptyR -> Nothing
    _ Seq.:> last -> Just last

-- 文法定义如下：
-- Prog :: (Bind | Anno)*
-- Anno :: var :: Ty
-- Bind :: var = Exp
--         | rec var = Exp
-- Exp :: var
--       | (Exp)
--       | lit
--       | \ var -> Exp
--       | let Bind in Exp   
--       | Exp Exp
--       | Exp :: Ty
-- Ty :: var
--     | Ty -> Ty
--     | (Ty)
--     | litT 

data Keywords = Let | In | Rec | Equal | Lamb | Arrow | TypeAnnot | LParen | RParen deriving (Eq, Show)

instance TShow Keywords where
    tshow :: Keywords -> T.Text
    tshow Let = "let"
    tshow In = "in"
    tshow Rec = "rec"
    tshow Equal = "="
    tshow Lamb = "\\"
    tshow Arrow = "->"
    tshow TypeAnnot = "::"
    tshow LParen = "("
    tshow RParen = ")"

__toKeyword :: T.Text -> Maybe Keywords
__toKeyword "let" = Just Let
__toKeyword "in" = Just In
__toKeyword "rec" = Just Rec
__toKeyword "=" = Just Equal
__toKeyword "\\" = Just Lamb
__toKeyword "->" = Just Arrow
__toKeyword "::" = Just TypeAnnot
__toKeyword "(" = Just LParen
__toKeyword ")" = Just RParen
__toKeyword _ = Nothing

-- toKeyword ::  S.Tokens s -> Maybe Keywords
-- toKeyword = __toKeyword . tshow

type LexcialR' = LexcialR T.Text Keywords

__refineLexcial :: forall s. (TShow (S.Tokens s), ?toKeyword::(S.Tokens s -> Maybe Keywords)) => Located (Lexcial s) -> Maybe (Located LexcialR')
__refineLexcial (Located (x, pos)) = do
    kw <- refineLexcial ?toKeyword x
    case kw of
        NumericLiteralR n -> Just $ Located (NumericLiteralR n, pos)
        StringLiteralR s -> Just $ Located (StringLiteralR (tshow s), pos)
        KeywordR k -> Just $ Located (KeywordR k, pos)
        IdentifierR k -> Just $ Located (IdentifierR (tshow k), pos)
        EOFR -> Just $ Located (EOFR, pos)

data LexerStream


instance S.TokenClass LexerStream where
    type Token LexerStream = Located LexcialR'
    type Tokens LexerStream = Seq (Located LexcialR')
    tokenLen = length
    fromList = Seq.fromList
    toList = toList

type Snap = (S.Tokens LexerStream, S.Tokens LexerStream) -- (已读, 待读)

runLexerStream :: (State Snap :> es, Input (S.Token LexerStream) :> es) => ParseEff s err (S.Stream LexerStream Snap : es) a -> ParseEff s1 err1 es a
runLexerStream = ParseEff . Hefty.interpret (\case
        S.TakeWhile p -> do
            (readed, unreaded) <- Hefty.get
            let (partsFromUnreaded, rest) = Seq.spanl p unreaded
            case rest of
                Seq.Empty -> do
                    -- go 函数返回两部分，前一部分是满足条件的词法单元序列，后一部分是第一个不满足条件的词法单元
                    let go = do
                            r <- Hefty.input
                            if p r then do
                                (_rest, _last) <- go
                                return (r Seq.<| _rest, _last)
                            else do
                                return (Seq.empty, r)
                    (newReaded, _last) <- go
                    Hefty.put (readed Seq.>< partsFromUnreaded Seq.>< newReaded, Seq.singleton _last)
                    return (partsFromUnreaded Seq.>< newReaded)
                _ -> do
                    Hefty.put (readed Seq.>< partsFromUnreaded, rest)
                    return partsFromUnreaded
        S.TakeN n -> do
            (readed, unreaded) <- Hefty.get
            let (partsFromUnreaded, rest) = Seq.splitAt n unreaded
            let readedLen = Seq.length readed
            if readedLen >= n then do
                Hefty.put (readed Seq.>< unreaded, rest)
                return partsFromUnreaded
            else do
                -- 需要从输入中继续读取
                let go m = if m < n then do
                        r <- Hefty.input
                        go (m + 1) <&> (r Seq.<|)
                    else return Seq.empty
                newReaded <- go (Seq.length partsFromUnreaded)
                Hefty.put (readed Seq.>< partsFromUnreaded Seq.>< newReaded, rest)
                return (partsFromUnreaded Seq.>< newReaded)
        S.Revert snap -> Hefty.put snap
        S.Current -> Hefty.get
    ) . asEff


data Anno vt v = Anno v (Typ TypeLitO vt)
    deriving (Eq, Show)

instance (TShow vt, TShow v) => TShow (Anno vt v) where
    tshow (Anno v t) = tshow v <> " :: " <> tshow t

-- partial typed AST
type Expr vt v = TypedTerm (Maybe (Typ TypeLitO vt)) LitO v

newtype TyVar = TyVar T.Text
    deriving (Eq, Show, TShow, IsText)

newtype TrmVar = TrmVar T.Text
    deriving (Eq, Show, TShow, IsText)

data Bind vt v = Bind v (Expr vt v)
    | RecBind v (Expr vt v)
    deriving (Eq, Show)

instance (TShow vt, TShow v) => TShow (Bind vt v) where
    tshow (Bind v e) = tshow v <> " = " <> tshow e
    tshow (RecBind v e) = "rec " <> tshow v <> " = " <> tshow e

type ParseCons snap s st err es = (
        ParseEffFOEWithTokens snap s st err es, TShow (S.Token s), Into (S.Token s) LexcialR'
    )

parseKeyword :: forall s snap st err es. (ParseCons snap s st err es) => Keywords -> ParseEff s err es ()
parseKeyword kw = withInStack' "When parsing keyword" $ void $ satisfy_ (\case
    KeywordR k | k == kw -> True
    _ -> False
    . (into :: S.Token s -> LexcialR')
    )

parseParened :: (ParseCons snap s st err es) =>
    ParseEff s err es a -> ParseEff s err es a
parseParened p = do
    parseKeyword LParen
    result <- p
    parseKeyword RParen
    return result

parseLitTy :: forall s snap st err es. (ParseCons snap s st err es) => ParseEff s err es (Typ TypeLitO TyVar)
parseLitTy =  withInStack' "When parsing literal type" do
    satisfy'_  (\case
        IdentifierR "Int" -> Just $ TLit TInt
        IdentifierR "String" -> Just $ TLit TString
        _ -> Nothing
        . (into :: S.Token s -> LexcialR')
        )

parseTyVar :: forall s snap st err es. (ParseCons snap s st err es) => ParseEff s err es (Typ TypeLitO TyVar)
parseTyVar = withInStack' "When parsing type variable" do
    satisfy'_  (\case
        IdentifierR s -> Just (TVar (TyVar s))
        _ -> Nothing
        . (into :: S.Token s -> LexcialR')
        )

-- Ty :: var
--     | Ty -> Ty
--     | (Ty)
--     | litT 

parseTy :: (ParseCons snap s st err es) => ParseEff s err es (Typ TypeLitO TyVar)
parseTy = withInStack' "When parsing type" do
    -- _head 是左公因子
    _head <- parseParened parseTy <|> parseLitTy <|> parseTyVar
    t <- lookAhead (parseKeyword Arrow)
    case t of
        Just () -> do
            TFun _head <$> parseTy
        Nothing -> return _head

-- Exp :: var
--       | (Exp)
--       | lit
--       | \ var -> Exp
--       | \ var :: Ty -> Exp
--       | let Bind in Exp   
--       | Exp Exp
--       | Exp :: Ty

parseTermVar :: forall s snap st err es t. (ParseCons snap s st err es, IsText t) => ParseEff s err es t
parseTermVar = withInStack' "When parsing var term" $ satisfy'_  (\case
        IdentifierR s ->  Just (fromText s)
        _ -> Nothing
        . (into :: S.Token s -> LexcialR')
        )

parseLit :: forall s snap st err es t. (ParseCons snap s st err es, LCTerm t) => ParseEff s err es t
parseLit = withInStack' "When parsing literal" $ satisfy'_  (\case
        NumericLiteralR n -> Just (intLit' n)
        StringLiteralR s -> Just (stringLit' s)
        _ -> Nothing
        . (into :: S.Token s -> LexcialR')
        )

type PTerm = PartialTypedTerm TypeLitO TyVar LitO TrmVar

parseAnno :: (ParseCons snap s st err es) => ParseEff s err es (Anno TyVar TrmVar)
parseAnno = withInStack' "When parsing annotation" do
    var <- parseTermVar
    parseKeyword TypeAnnot
    Anno (TrmVar var) <$> parseTy

parseLam :: (ParseCons snap s st err es) => ParseEff s err es PTerm
parseLam =  withInStack' "When parsing lambda" do
    parseKeyword Lamb
    var <- parseTermVar
    opTy <- optional (parseKeyword TypeAnnot >> parseTy)
    parseKeyword Arrow
    r <- parseExp
    case typOfTTerm r of
        Just t -> return $ LamT var opTy r (fmap (`TFun` t) opTy)
        Nothing -> return $ LamT var opTy r Nothing


parseBind :: forall snap s st err es. (ParseCons snap s st err es) => ParseEff s err es (Bind TyVar TrmVar)
parseBind = withInStack' "When parsing bind" do
    -- rec_ <- optional (parseKeyword @s Rec)
    let rec_ = Nothing
    var <- parseTermVar
    parseKeyword Equal
    body <- parseExp
    case rec_ of
        Just () -> return $ RecBind var body
        Nothing -> return $ Bind var body

parseExp :: (ParseCons snap s st err es) => ParseEff s err es PTerm
parseExp = withInStack' "When parsing expression" do
    _head <- parseLam <|> parseLit <|> fmap varTerm parseTermVar <|> parseParened parseExp <|> parseLet
    (   do
        parseKeyword TypeAnnot
        ty <- parseTy
        return $ annotating ty _head
        )
        <|> fmap unannotating (appTerm _head <$> parseExp)
        <|> return (unannotating _head)
    where
        parseLet :: (ParseCons snap s st err es) => ParseEff s err es PTerm
        parseLet = do
            parseKeyword Let
            bind <- parseBind
            parseKeyword In
            body <- parseExp
            case bind of
                Bind v e -> return $ OLetT v Nothing e body (typOfTTerm e)
                RecBind v e -> return $ RLetT v Nothing e body (typOfTTerm e)

newtype ParserError = ParserError T.Text deriving (Show, Eq, IsText, TShow, Pretty)

type ParserES s1 = '[
        S.Stream LexerStream Snap,
        Input (S.Token LexerStream),
        MultiThrow (ParserError, Snap),
        Hefty.Ask Snap,
        State Snap,
        Input (Located (Lexcial s1))
        ]

refineLexcialInput :: (Input (Located (Lexcial s1)) :> es, TShow (S.Tokens s1), ?toKeyword::S.Tokens s1 -> Maybe Keywords) =>
    ParseEff s err (Input (S.Token LexerStream) : es) a
    -> ParseEff s err es a
refineLexcialInput = liftP $
    Hefty.runInputEff go
    where
        go = do
            result <- Hefty.input
            maybe go return (__refineLexcial result)

runStatefulThrowWithSnap :: (Hefty.FOEs es, HasSourceViewer es) => ParseEff s ParserError (MultiThrow (ParserError, Snap) : es) a -> ParseEff s ParserError es (Either Doc a)
runStatefulThrowWithSnap p = do
    result <- runThrow p
    case result of
        Left errorTree -> do
            let errorTree1 = fmap (fmap (\(e, cur_snap) ->
                    (pretty e,
                    case seqLast (fst cur_snap) of
                        Just (Located (_, pos)) -> pos
                        Nothing -> (dummyPos, dummyPos) -- 这种情况理论上不应该发生
                    ))) errorTree
            msg <- printErrorForestE errorTree1
            return $ Left msg
        Right r -> return $ Right r

runSimpleParser :: (Hefty.FOEs es, HasSourceViewer es, S.Tokens s ~ T.Text) => ParseEff s err es (Located (Lexcial s)) -> ParseEff LexerStream ParserError (ParserES s ++ es) a -> ParseEff s err es (Either Doc a)
runSimpleParser _lexer =
    stream' _lexer .
    liftP (Hefty.evalState (Seq.empty, Seq.empty)) .
    liftP (\p -> Hefty.get >>= \r -> Hefty.runAsk r p) .
    runStatefulThrowWithSnap .
    refineLexcialInput .
    runLexerStream
    where ?toKeyword = __toKeyword
