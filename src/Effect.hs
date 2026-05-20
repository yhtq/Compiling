{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Effect where
import Text
import qualified Control.Monad.Hefty as Hefty
import qualified Stream as S
import qualified Control.Effect as CE
import Data.HashSet ()
import Control.Monad.Hefty (
    type (++),
    FOEs,
    Eff,
    State,
    interpret,
    type (:>),
    type (~>),
    (&)
 )

import Data.Effect.OpenUnion (KnownLength)
import qualified Control.Monad.Hefty.State as Hefty
import Data.Maybe (isNothing)
import Control.Applicative (Alternative(..))
import Data.Coerce (coerce)
import Data.Foldable (asum)
import qualified Data.Text as T
import qualified Exception as E
import Exception (StatefulThrow)
import qualified Data.Tree as T
import Control.Monad.Hefty.Input (Input)
import qualified Control.Monad.Hefty.Input as Hefty
import Control.Monad (void)

newtype ParserST s f a = ParserST (State s f a)
type instance Hefty.OrderOf (ParserST s) = Hefty.OrderOf (State s)
type instance Hefty.LabelOf (ParserST s) = Hefty.LabelOf (State s)
type instance Hefty.FormOf (ParserST s) = Hefty.FormOf (State s)
deriving instance Hefty.PolyHFunctor (ParserST s)
deriving instance Hefty.FirstOrder (ParserST s)

-- s 是流类型，buf 是流类型使用的 snap
-- st 是异常节点中携带的状态类型，err 是异常节点的类型
-- es 是 ParseEff 中使用的 effect 列表
type ParseEffConstraints snap s st err es = (S.Stream s snap :> es, StatefulThrow err st es, IsText err)
type ParseEffFOEConstraints snap s st err es = (S.Stream s snap :> es, StatefulThrow err st es, IsText err, FOEs es)
type ParseEffFOEWithTokens snap s st err es = (S.TokenClass s, ParseEffFOEConstraints snap s st err es)

type TokenConstraints s = (TShow (S.Token s), Eq (S.Token s))
type TokensConstraints s = (TShow (S.Tokens s), Eq (S.Tokens s))

newtype ParseEff s err es a = ParseEff (Eff es a)

deriving instance Functor (ParseEff s err es)
deriving instance Applicative (ParseEff s err es)
deriving instance Monad (ParseEff s err es)

-- newtype CallStack = CallStack T.Text deriving (Show, Eq, IsString, Semigroup, Monoid)
-- type CallStacks = [CallStack]

-- type HasParserCallStack es = CallStackE :> es

-- data CallStackE :: Effect where
--     Push :: CallStack -> CallStackE f ()
--     Pop :: CallStackE f ()
-- makeEffectF ''CallStackE


-- pushCallStack :: (HasParserCallStack es) => CallStack -> ParseEff s err es ()
-- pushCallStack cs = ParseEff $ Hefty.perform (Push cs)

-- popCallStack :: (HasParserCallStack es) => ParseEff s err es ()
-- popCallStack = ParseEff $ Hefty.perform Pop

-- withCallStack :: (HasParserCallStack es) => CallStack -> ParseEff s err es a -> ParseEff s err es a
-- withCallStack cs p = do
--     pushCallStack cs 
--     result <- p
--     _ <- popCallStack
--     return result


{-# INLINE asEff #-}
asEff :: ParseEff s err es a -> Eff es a
asEff = coerce

{-# INLINE liftP #-}
liftP :: (Eff es1 a -> Eff es2 b) -> ParseEff s err es1 a -> ParseEff s err es2 b
liftP = coerce

{-# INLINE getParserState #-}
getParserState :: (ParserST s :> es) => ParseEff s err es s
getParserState = ParseEff $ Hefty.perform (ParserST Hefty.Get)

{-# INLINE putParserState #-}
putParserState :: (ParserST s :> es) => s -> ParseEff s err es ()
putParserState s = ParseEff $ Hefty.perform (ParserST (Hefty.Put s))

{-# INLINE runParserST #-}
runParserST :: (FOEs es) => ParseEff s err (ParserST s : es) a -> s -> ParseEff s err es (s, a)
runParserST (ParseEff p) initState = ParseEff $ Hefty.runState initState (
        interpret (\(ParserST e) -> 
            Hefty.perform e
        ) (Hefty.raiseUnder p)
    )

{-# INLINE runPureParseEff #-}
runPureParseEff :: ParseEff s err '[] a -> a
runPureParseEff (ParseEff p) = Hefty.runPure p



{-# INLINE raise #-}
raise :: ParseEff s1 err1 es a -> ParseEff s2 err2 (e : es) a
raise (ParseEff p) = ParseEff $ Hefty.raise p 

{-# INLINE raiseUnder #-}
raiseUnder :: ParseEff s1 err1 (e0 : es) a -> ParseEff s2 err2 (e0 : e1 : es) a
raiseUnder = ParseEff . Hefty.raiseUnder . asEff

{-# INLINE throw #-}
throw :: (StatefulThrow err st es) => err -> ParseEff s err es a
throw = ParseEff . E.throw

{-# INLINE reThrow #-}
reThrow :: (E.MultiThrow node :> es) => E.MultiThrow node (Eff es) a -> ParseEff s err es b
reThrow = ParseEff . E.reThrow

{-# INLINE catch #-}
catch :: (E.MultiThrow node :> es, FOEs es) => ParseEff s err es a -> (forall x. E.MultiThrow node (Eff es) x -> ParseEff s err es a) -> ParseEff s err es a
catch (ParseEff action) handler = ParseEff $ E.catch action $ asEff . handler


{-# INLINE withInStack #-}
withInStack :: (StatefulThrow err st es, FOEs es) => err -> ParseEff s err es a -> ParseEff s err es a
withInStack err action = catch action $ \e -> ParseEff $ do
    node <- E.inState err
    E.reThrow $ E.TreeThrow node e

{-# INLINE withInStack' #-}
withInStack' :: (StatefulThrow err st es, FOEs es, IsText err) => T.Text -> ParseEff s err es a -> ParseEff s err es a
withInStack' errText = withInStack (fromText errText)

{-# INLINE runThrow #-}
runThrow :: (FOEs es) => ParseEff s err (E.MultiThrow node : es) a -> ParseEff s err es (Either (T.Forest node) a)
runThrow = ParseEff . 
    Hefty.interpretBy (pure . Right) (\e _ -> 
        pure $ Left (E.translateToTree e)
    ) . asEff


-- 下面的接口参考了 megaparsec 的接口设计，提供了一些基本的 parser combinator 接口
{-# INLINE[2] try #-}
try :: (ParseEffFOEConstraints snap s st err es) => ParseEff s err es a -> ParseEff s err es a
try p = do
    s <- ParseEff S.current
    catch p (\e -> do
                ParseEff $ S.revert s
                reThrow e
            )

{-# INLINE compact1 #-}
compact1 :: forall e s es err. (e :> es) => ParseEff s err (e : es)  ~> ParseEff s err es
compact1 = ParseEff . Hefty.translate id . asEff

-- {-# INLINE[2] observing' #-}
-- observing' :: (FOEs es) => ParseEff s err (E.MultiThrow node : es) a -> ParseEff s err es (Either (T.Forest node) a)
-- observing' = runThrow

{-# INLINE[2] observing #-}
observing :: (FOEs es, E.MultiThrow node :> es) => ParseEff s err es a -> ParseEff s err es (Either (T.Forest node) a)
observing = ParseEff . Hefty.interposeBy (pure . Right) (\e _ -> pure $ Left (E.translateToTree e)) . asEff

{-# RULES 
    "try observing" forall p. try (observing p) = observing p
#-}

{-# INLINE[2] _empty #-}
_empty :: ParseEffConstraints snap s st err es => ParseEff s err es a
_empty = compact1 empty'

{-# INLINE empty' #-}
empty' :: (StatefulThrow err st es, IsText err) => ParseEff s err (E.MultiThrow (err, st) : es) a
empty' = throw (fromText "empty")

{-# INLINE[2] _alter #-}
_alter :: (ParseEffFOEConstraints snap s st err es) => ParseEff s err es a -> ParseEff s err es a -> ParseEff s err es a
_alter (ParseEff p1) (ParseEff p2) = ParseEff $ E.alter p1 p2


-- many' :: (ParseEffFOEConstraints snap s st err es) => ParseEff s err (E.MultiThrow (err, st) : es) a -> ParseEff s err es [a]
-- many' p = withInStack' "many" do
--     s <- ParseEff S.current
--     next <- withInStack' "observing" $ observing' p
--     case next of
--         Left _ -> 
--             ParseEff $ S.revert s >>
--             return []
--         Right r -> (r :) <$> many' p

{-# INLINE[2] _many #-}
_many :: (ParseEffFOEConstraints snap s st err es) => ParseEff s err es a -> ParseEff s err es [a]
_many p = withInStack' "many" $ do
    s <- ParseEff S.current
    next <- withInStack' "observing" $ observing p
    case next of
        Left _ -> do
            ParseEff $ S.revert s 
            return []
        Right r -> (r :) <$> _many p

-- 注意使用 <|>, some, many, optional 等组合子时 *失败时不会自动回溯*
instance (ParseEffFOEConstraints snap s st err es) => Alternative (ParseEff s err es) where
    {-# INLINE empty #-}
    empty = _empty
    {-# INLINE (<|>) #-}
    p1 <|> p2 = _alter p1 p2
    {-# INLINE many #-}
    many = _many

{-# RULES 
"try empty" try _empty = _empty
"many empty" many _empty = return []
#-}
{-# RULES 
"alter empty" forall p. _alter _empty p = p
"empty alter" forall p. _alter p _empty = p
#-}
{-# RULES 
"alter observing" forall p1 p2. _alter (observing p1) p2 = observing p1
"alter many" forall p1 p2. _alter (_many p1) p2 = _many p1
#-}
{-# RULES 
"try many" forall p. try (_many p) = _many p
#-}

{-# INLINE compact #-}
compact :: forall es. (KnownLength es) => Eff (es ++ es) ~> Eff es
compact = Hefty.interprets @es (
            CE.Eff . Hefty.liftFree 
        )

{-# INLINE (<>+) #-}
(<>+) :: forall s err a b es1 es2. 
            (KnownLength es1)
        => ParseEff s err es1 a -> ParseEff s err es2 b -> ParseEff s err (es1 ++ es2) (a, b)
(<>+) (ParseEff p1) (ParseEff p2) = do
    ar <- ParseEff $ (Hefty.raiseSuffix @es2) p1
    br <- ParseEff $ (Hefty.raisePrefix @es1) p2
    return (ar, br)


{-# INLINE (<|>+) #-}
(<|>+) :: forall snap s st err a b es1 es2. 
            (KnownLength es1, ParseEffConstraints snap s st err (es1 ++ es2), FOEs (es1 ++ es2))
        => ParseEff s err es1 a -> ParseEff s err es2 b -> ParseEff s err (es1 ++ es2) (Either a b)
(<|>+) (ParseEff p1) (ParseEff p2) = do
    catch 
        (ParseEff $ (Hefty.raiseSuffix @es2) (Left <$> p1)) 
        (\_ -> ParseEff $ (Hefty.raisePrefix @es1) (Right <$> p2))

{-# INLINE[2] lookAhead #-}
lookAhead :: (ParseEffFOEConstraints snap s st err es) => ParseEff s err es a -> ParseEff s err es (Maybe a)
lookAhead p = do
    s <- ParseEff S.current
    next <- observing p
    ParseEff $ S.revert s
    case next of
        Left _ -> return Nothing
        Right r -> return (Just r)

{-# RULES 
    "satisfy or" forall p1 p2. satisfy p1 <|> satisfy p2 = satisfy (\t -> p1 t || p2 t) 
#-}

-- 以下单个字符的接口在失败时都不会消耗流，因此不需要配合 `try` 使用

{-# INLINE[2] satisfy #-}
satisfy :: (ParseEffFOEWithTokens snap s st err es, TShow (S.Token s)) => (Maybe (S.Token s) -> Bool) -> ParseEff s err es (Maybe (S.Token s))
satisfy p = withInStack' "satisfy" $ do
    h <- ParseEff S.peek
    if p h then 
        ParseEff S.head >>
        return h 
    else 
        throw (fromText ("Unexpected token: " <> tshow h))

{-# RULES 
    "satisfy_ or" forall p1 p2. satisfy_ p1 <|> satisfy_ p2 = satisfy_ (\t -> p1 t || p2 t) 
#-}

-- satisfy 系列函数失败时不会有任何消耗

{-# INLINE[2] satisfy_ #-}
-- fail on EOF
satisfy_ :: (ParseEffFOEWithTokens snap s st err es, TShow (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Token s)
satisfy_ p = withInStack' "satisfy" $ do
    h <- ParseEff S.peek
    case h of
        Just t | p t -> 
            ParseEff S.head >>
            return t
        _ -> throw (fromText ("Unexpected token: " <> tshow h))

{-# INLINE[2] satisfy'_ #-}
satisfy'_ :: (ParseEffFOEWithTokens snap s st err es, TShow (S.Token s)) => (S.Token s -> Maybe a) -> ParseEff s err es a
satisfy'_ p = withInStack' "satisfy_" $ do
    h <- ParseEff S.peek
    case h of
        Just t | Just a <- p t -> 
            ParseEff S.head >>
            return a
        _ -> throw (fromText ("Unexpected token: " <> tshow h))


{-# INLINE[2] anyOf #-}
anyOf :: (ParseEffFOEConstraints snap s st err es) => [ParseEff s err es a] -> ParseEff s err es a
anyOf = asum

{-# RULES 
"try satisfy" forall p. try (satisfy p) = satisfy p
"try satisfy_" forall p. try (satisfy_ p) = satisfy_ p
"try eof" try eof = eof
"anyOf satisfy_" forall pl. anyOf (map satisfy_ pl) = satisfy_ (\x -> any (x &) pl)
"anyOf satisfy" forall pl. anyOf (map satisfy pl) = satisfy (\x -> any (x &) pl)
#-}

{-# INLINE[2] eof #-}
eof :: (ParseEffFOEWithTokens snap s st err es, TShow (S.Token s)) => ParseEff s err es ()
eof = withInStack' "eof" $ do
    hs <- ParseEff $ S.peek
    if isNothing $ hs then 
        void $ ParseEff S.head  
    else throw $ fromText ("Expected end of input, but got " <> tshow hs)

-- 注意 tokens 接口在失败时会消耗流中的部分 token，因此在使用时需要注意回溯问题，建议配合 `try` 使用
{-# INLINE tokens #-}
tokens :: forall snap s st err es. (ParseEffFOEWithTokens snap s st err es, TokensConstraints s) => S.Tokens s -> ParseEff s err es ()
tokens ts = withInStack' "tokens" $ do
    heads <- ParseEff $ S.takeN (S.tokenLen @s ts) 
    if heads == ts then do
        return ()
    else
        throw $ fromText ("Expected " <> tshow ts <> ", but got " <> tshow heads)

{-# RULES 
    "many satisfy" forall p. many (satisfy_ p) = takeWhileP' p
#-}

{-# INLINE takeWhileP #-}
takeWhileP :: (ParseEffFOEWithTokens snap s st err es) => (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
takeWhileP p = withInStack' "takeWhileP" $ do
    ParseEff $ S.takeWhile p 
{-# INLINE takeWhileP' #-}
takeWhileP' :: forall snap s st err es. (ParseEffFOEWithTokens snap s st err es) => (S.Token s -> Bool) -> ParseEff s err es [S.Token s]
takeWhileP' p = fmap (S.toList @s) (takeWhileP p)

{-# INLINE takeWhileP1 #-}
takeWhileP1 :: forall snap s st err es.  (ParseEffFOEWithTokens snap s st err es, TShow (S.Token s), Monoid (S.Tokens s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
takeWhileP1 p = do 
    h <- satisfy_ p 
    t <- takeWhileP p
    return $ S.fromList @s [h] <> t

{-# INLINE takeWhileP1' #-}
takeWhileP1' :: forall snap s st err es. (ParseEffFOEWithTokens snap s st err es, TShow (S.Token s), Monoid (S.Tokens s)) => (S.Token s -> Bool) -> ParseEff s err es [S.Token s]
takeWhileP1' p = fmap (S.toList @s) (takeWhileP1 p)

-- Coroutine version of `many`, used for fusion different passes
{-# INLINE stream #-}
stream :: (Hefty.Suffix es es') =>
    ParseEff s err es a -> ParseEff s1 err1 (Input a : es') x -> Eff es' x
stream p = Hefty.interpret (
        \Hefty.Input -> Hefty.raises $ asEff p
    ) . asEff     


{-# INLINE stream' #-}
stream' :: (Hefty.Suffix es es') =>
    ParseEff s err es a -> ParseEff s1 err1 (Input a : es') x -> ParseEff s err es' x
stream' p q = ParseEff (stream p q) 

