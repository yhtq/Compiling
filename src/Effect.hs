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

newtype ParserST s f a = ParserST (State s f a)
type instance Hefty.OrderOf (ParserST s) = Hefty.OrderOf (State s)
type instance Hefty.LabelOf (ParserST s) = Hefty.LabelOf (State s)
type instance Hefty.FormOf (ParserST s) = Hefty.FormOf (State s)
deriving instance Hefty.PolyHFunctor (ParserST s)
deriving instance Hefty.FirstOrder (ParserST s)

type ParseEffConstraints buf s st err es = (S.Stream s buf :> es, StatefulThrow err st es, IsText err)
type ParseEffFOEConstraints buf s st err es = (S.Stream s buf :> es, StatefulThrow err st es, IsText err, FOEs es)
type ParseEffFOEWithTokens buf s st err es = (S.TokenClass s, ParseEffFOEConstraints buf s st err es)

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
try :: (ParseEffFOEConstraints buf s st err es) => ParseEff s err es a -> ParseEff s err es a
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
_empty :: ParseEffConstraints buf s st err es => ParseEff s err es a
_empty = compact1 empty'

{-# INLINE empty' #-}
empty' :: (StatefulThrow err st es, IsText err) => ParseEff s err (E.MultiThrow (err, st) : es) a
empty' = throw (fromText "empty")

{-# INLINE[2] _alter #-}
_alter :: (ParseEffFOEConstraints buf s st err es) => ParseEff s err es a -> ParseEff s err es a -> ParseEff s err es a
_alter (ParseEff p1) (ParseEff p2) = ParseEff $ E.alter p1 p2


-- many' :: (ParseEffFOEConstraints buf s st err es) => ParseEff s err (E.MultiThrow (err, st) : es) a -> ParseEff s err es [a]
-- many' p = withInStack' "many" do
--     s <- ParseEff S.current
--     next <- withInStack' "observing" $ observing' p
--     case next of
--         Left _ -> 
--             ParseEff $ S.revert s >>
--             return []
--         Right r -> (r :) <$> many' p

{-# INLINE[2] _many #-}
_many :: (ParseEffFOEConstraints buf s st err es) => ParseEff s err es a -> ParseEff s err es [a]
_many p = withInStack' "many" $ do
    s <- ParseEff S.current
    next <- withInStack' "observing" $ observing p
    case next of
        Left _ -> do
            ParseEff $ S.revert s 
            return []
        Right r -> (r :) <$> _many p

-- 注意使用 <|>, some, many, optional 等组合子时 *失败时不会自动回溯*
instance (ParseEffFOEConstraints buf s st err es) => Alternative (ParseEff s err es) where
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
(<|>+) :: forall buf s st err a b es1 es2. 
            (KnownLength es1, ParseEffConstraints buf s st err (es1 ++ es2), FOEs (es1 ++ es2))
        => ParseEff s err es1 a -> ParseEff s err es2 b -> ParseEff s err (es1 ++ es2) (Either a b)
(<|>+) (ParseEff p1) (ParseEff p2) = do
    catch 
        (ParseEff $ (Hefty.raiseSuffix @es2) (Left <$> p1)) 
        (\_ -> ParseEff $ (Hefty.raisePrefix @es1) (Right <$> p2))

{-# INLINE[2] lookAhead #-}
lookAhead :: (ParseEffFOEConstraints buf s st err es) => ParseEff s err es a -> ParseEff s err es (Maybe a)
lookAhead p = do
    s <- ParseEff S.current
    next <- observing p
    ParseEff $ S.revert s
    case next of
        Left _ -> return Nothing
        Right r -> return (Just r)

{-# RULES 
    "satisfy or" forall p1 p2. try (satisfy p1) <|> try (satisfy p2) = try $ satisfy (\t -> p1 t || p2 t) 
#-}

{-# INLINE[2] satisfy #-}
satisfy :: (ParseEffFOEWithTokens buf s st err es, TShow (S.Token s)) => (Maybe (S.Token s) -> Bool) -> ParseEff s err es (Maybe (S.Token s))
satisfy p = withInStack' "satisfy" $ do
    h <- ParseEff S.head
    if p h then return h else throw (fromText ("Unexpected token: " <> tshow h))

{-# RULES 
    "satisfy_ or" forall p1 p2. try (satisfy_ p1) <|> try (satisfy_ p2) = try $ satisfy_ (\t -> p1 t || p2 t) 
#-}


{-# INLINE[2] satisfy_ #-}
-- fail on EOF
satisfy_ :: (ParseEffFOEWithTokens buf s st err es, TShow (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Token s)
satisfy_ p = withInStack' "satisfy" $ do
    h <- ParseEff S.head
    case h of
        Just t | p t -> return t
        _ -> throw (fromText ("Unexpected token: " <> tshow h))

{-# INLINE[2] anyOf #-}
anyOf :: (ParseEffFOEConstraints buf s st err es) => [ParseEff s err es a] -> ParseEff s err es a
anyOf = asum

{-# RULES 
"anyOf satisfy_" forall pl. anyOf (map (try . satisfy_) pl) = try $ satisfy_ (\x -> any (x &) pl)
"anyOf satisfy" forall pl. anyOf (map (try . satisfy) pl) = try $ satisfy (\x -> any (x &) pl)
#-}

{-# INLINE[2] eof #-}
eof :: (ParseEffFOEWithTokens buf s st err es, TShow (S.Token s)) => ParseEff s err es ()
eof = withInStack' "eof" $ do
    hs <- ParseEff $ S.head
    if isNothing $ hs then return () else throw $ fromText ("Expected end of input, but got " <> tshow hs)

{-# INLINE tokens #-}
tokens :: forall buf s st err es. (ParseEffFOEWithTokens buf s st err es, TokensConstraints s) => S.Tokens s -> ParseEff s err es ()
tokens ts = withInStack' "tokens" $ do
    heads <- ParseEff $ S.takeN (S.tokenLen @s ts) 
    if heads == ts then do
        return ()
    else
        throw $ fromText ("Expected " <> tshow ts <> ", but got " <> tshow heads)

{-# RULES 
    "many satisfy" forall p. many (try $ satisfy_ p) = takeWhileP' p
#-}

{-# INLINE takeWhileP #-}
takeWhileP :: (ParseEffFOEWithTokens buf s st err es) => (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
takeWhileP p = withInStack' "takeWhileP" $ do
    ParseEff $ S.takeWhile p 
{-# INLINE takeWhileP' #-}
takeWhileP' :: forall buf s st err es. (ParseEffFOEWithTokens buf s st err es) => (S.Token s -> Bool) -> ParseEff s err es [S.Token s]
takeWhileP' p = fmap (S.toList @s) (takeWhileP p)

{-# INLINE takeWhileP1 #-}
takeWhileP1 :: forall buf s st err es.  (ParseEffFOEWithTokens buf s st err es, TShow (S.Token s), Monoid (S.Tokens s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
takeWhileP1 p = do 
    h <- satisfy_ p 
    t <- takeWhileP p
    return $ S.fromList @s [h] <> t

{-# INLINE takeWhileP1' #-}
takeWhileP1' :: forall buf s st err es. (ParseEffFOEWithTokens buf s st err es, TShow (S.Token s), Monoid (S.Tokens s)) => (S.Token s -> Bool) -> ParseEff s err es [S.Token s]
takeWhileP1' p = fmap (S.toList @s) (takeWhileP1 p)

-- Coroutine version of `many`, used for fusion different passes
{-# INLINE stream #-}
stream :: (Hefty.Suffix es es') =>
    ParseEff s err es a -> ParseEff s err (Input a : es') x -> ParseEff s err es' x
stream p = ParseEff . Hefty.interpret (
        \Hefty.Input -> Hefty.raises $ asEff p
    ) . asEff 
