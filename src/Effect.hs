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

newtype ParserST s f a = ParserST (State s f a)
type instance Hefty.OrderOf (ParserST s) = Hefty.OrderOf (State s)
type instance Hefty.LabelOf (ParserST s) = Hefty.LabelOf (State s)
type instance Hefty.FormOf (ParserST s) = Hefty.FormOf (State s)
deriving instance Hefty.PolyHFunctor (ParserST s)
deriving instance Hefty.FirstOrder (ParserST s)

type ParseEffConstraints s st err es = (ParserST s :> es, StatefulThrow err st es, IsText err)
type ParseEffFOEConstraints s st err es = (ParserST s :> es, StatefulThrow err st es, IsText err, FOEs es)

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
try :: (ParseEffFOEConstraints s st err es) => ParseEff s err es a -> ParseEff s err es a
try p = do
    s <- getParserState
    catch p (\e -> do
                putParserState s
                reThrow e
            )

{-# INLINE compact1 #-}
compact1 :: forall e s es err. (e :> es) => ParseEff s err (e : es)  ~> ParseEff s err es
compact1 = ParseEff . Hefty.translate id . asEff

{-# INLINE[2] observing' #-}
observing' :: (FOEs es) => ParseEff s err (E.MultiThrow node : es) a -> ParseEff s err es (Either (T.Forest node) a)
observing' = runThrow

{-# INLINE[2] observing #-}
observing :: (FOEs es) => ParseEff s err es a -> ParseEff s err es (Either (T.Forest node) a)
observing = observing' . raise

{-# RULES 
    "try observing" forall p. try (observing p) = observing p
#-}

{-# INLINE[2] _empty #-}
_empty :: ParseEffConstraints s st err es => ParseEff s err es a
_empty = compact1 empty'

{-# INLINE empty' #-}
empty' :: (StatefulThrow err st es, IsText err) => ParseEff s err (E.MultiThrow (err, st) : es) a
empty' = throw (fromText "empty")

{-# INLINE[2] _alter #-}
_alter :: (ParseEffFOEConstraints s st err es) => ParseEff s err es a -> ParseEff s err es a -> ParseEff s err es a
_alter (ParseEff p1) (ParseEff p2) = ParseEff $ E.alter p1 p2


many' :: (ParserST s :> es, StatefulThrow err st es, IsText err, FOEs es) => ParseEff s err (E.MultiThrow (err, st) : es) a -> ParseEff s err es [a]
many' p = withInStack' "many" do
    s <- getParserState
    next <- observing' p
    case next of
        Left _ -> 
            putParserState s >>
            return []
        Right r -> (r :) <$> many' p

{-# INLINE[2] _many #-}
_many :: (ParseEffFOEConstraints s st err es) => ParseEff s err es a -> ParseEff s err es [a]
_many = many' . raise

instance (ParseEffFOEConstraints s st err es) => Alternative (ParseEff s err es) where
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
(<|>+) :: forall s st err a b es1 es2. 
            (KnownLength es1, ParseEffConstraints s st err (es1 ++ es2), FOEs (es1 ++ es2))
        => ParseEff s err es1 a -> ParseEff s err es2 b -> ParseEff s err (es1 ++ es2) (Either a b)
(<|>+) (ParseEff p1) (ParseEff p2) = do
    catch 
        (ParseEff $ (Hefty.raiseSuffix @es2) (Left <$> p1)) 
        (\_ -> ParseEff $ (Hefty.raisePrefix @es1) (Right <$> p2))

{-# INLINE[2] lookAhead #-}
lookAhead :: (ParseEffFOEConstraints s st err es) => ParseEff s err es a -> ParseEff s err es (Maybe a)
lookAhead p = do
    s <- getParserState
    next <- observing p
    putParserState s
    case next of
        Left _ -> return Nothing
        Right r -> return (Just r)

{-# RULES 
    "satisfy or" forall p1 p2. satisfy p1 <|> satisfy p2 = satisfy (\t -> p1 t || p2 t) 
#-}

{-# INLINE[2] satisfy #-}
satisfy :: (ParseEffFOEConstraints s st err es, S.Stream s, TShow (S.Token s)) => (Maybe (S.Token s) -> Bool) -> ParseEff s err es (Maybe (S.Token s))
satisfy p = withInStack' "satisfy" $ do
    s <- getParserState
    if p (S.head s) then do
        putParserState (S.tail s)
        return $ S.head s
    else
        throw (fromText ("Unexpected token: " <> tshow (S.head s)))

{-# RULES 
    "satisfy_ or" forall p1 p2. satisfy_ p1 <|> satisfy_ p2 = satisfy_ (\t -> p1 t || p2 t) 
#-}


{-# INLINE[2] satisfy_ #-}
-- fail on EOF
satisfy_ :: (ParseEffFOEConstraints s st err es, S.Stream s, TShow (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Token s)
satisfy_ p = withInStack' "satisfy" $ do
    s <- getParserState
    case S.head s of
        Just t | p t -> do
            putParserState (S.tail s)
            return t
        _ -> throw $ fromText ("Unexpected token: " <> tshow (S.head s))

{-# INLINE[2] anyOf #-}
anyOf :: (ParseEffFOEConstraints s st err es) => [ParseEff s err es a] -> ParseEff s err es a
anyOf = withInStack' "anyOf" . asum

{-# RULES 
"anyOf satisfy_" forall pl. anyOf (map satisfy_ pl) = satisfy_ (\x -> any (x &) pl)
"anyOf satisfy" forall pl. anyOf (map satisfy pl) = satisfy (\x -> any (x &) pl)
#-}

{-# INLINE[2] eof #-}
eof :: (ParseEffFOEConstraints s st err es, S.Stream s, TShow (S.Token s)) => ParseEff s err es ()
eof = withInStack' "eof" $ do
    s <- getParserState
    if isNothing $ S.head s then return () else throw $ fromText ("Expected end of input, but got " <> tshow (S.head s))

{-# INLINE tokens #-}
tokens :: forall s st err es. (ParseEffFOEConstraints s st err es, S.Stream s, TokensConstraints s) => S.Tokens s -> ParseEff s err es ()
tokens ts = withInStack' "tokens" $ do
    s <- getParserState
    let (heads, rest) = S.takeN_ (S.tokenLen @s ts) s
    if heads == ts then do
        putParserState rest
        return ()
    else
        throw $ fromText ("Expected " <> tshow ts <> ", but got " <> tshow heads)

{-# RULES 
    "many satisfy" forall p. many (satisfy_ p) = takeWhileP' p
#-}
{-# RULES 
    "some satisfy" forall p. some (satisfy_ p) = takeWhileP1' p
#-}
{-# INLINE takeWhileP #-}
takeWhileP :: (ParseEffFOEConstraints s st err es, S.Stream s) => (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
takeWhileP p = withInStack' "takeWhileP" $ do
    s <- getParserState
    let (ts, rest) = S.takeWhile_ p s
    putParserState rest
    return ts
{-# INLINE takeWhileP' #-}
takeWhileP' :: forall s st err es. (ParseEffFOEConstraints s st err es, S.Stream s) => (S.Token s -> Bool) -> ParseEff s err es [S.Token s]
takeWhileP' p = fmap (S.toList @s) (takeWhileP p)

{-# INLINE takeWhileP1 #-}
takeWhileP1 :: (ParseEffFOEConstraints s st err es, S.Stream s, TShow (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
takeWhileP1 p = satisfy_ p >> takeWhileP p

{-# INLINE takeWhileP1' #-}
takeWhileP1' :: forall s st err es. (ParseEffFOEConstraints s st err es, S.Stream s, TShow (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es [S.Token s]
takeWhileP1' p = fmap (S.toList @s) (takeWhileP1 p)

