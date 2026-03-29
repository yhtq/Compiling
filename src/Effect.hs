{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Effect where
import qualified Control.Monad.Hefty as Hefty
import qualified Stream as S
import qualified Control.Effect as CE
import Data.HashSet ()
import Control.Monad.Hefty (
    type (++),
    AlgHandler,
    Throw(..),
    Catch(..),
    FOEs,
    reinterpret,
    reinterpretWith,
    Eff(..),
    Effect,
    Emb,
    State,
    interprets,
    interpret,
    interpretBy,
    interpretWith,
    liftIO,
    makeEffectF,
    makeEffectH,
    transform,
    type (:>),
    type (~>),
    (&), rewrite, Ask(..), In, type (~~>)
 )
import Control.Monad.Hefty.Except (catch)
import qualified Control.Monad.Hefty.Except as Hefty
import Data.Effect.OpenUnion (KnownLength, union)
import qualified Control.Monad.Hefty.State as Hefty
import Data.String (IsString(fromString))
import Data.Maybe (isNothing)
import Control.Applicative (Alternative(..))
import Data.Coerce (coerce)
import Stream (TokenConstraints)
import Data.Foldable (asum)

newtype ParserST s f a = ParserST (State s f a)
type instance Hefty.OrderOf (ParserST s) = Hefty.OrderOf (State s)
type instance Hefty.LabelOf (ParserST s) = Hefty.LabelOf (State s)
type instance Hefty.FormOf (ParserST s) = Hefty.FormOf (State s)
deriving instance Hefty.PolyHFunctor (ParserST s)
deriving instance Hefty.FirstOrder (ParserST s)

type ParseEffConstraints s err es = (FOEs es, ParserST s :> es, Throw err :> es, IsString err)
newtype ParseEff s err es a = ParseEff (Eff es a)

deriving instance Functor (ParseEff s err es)
deriving instance Applicative (ParseEff s err es)
deriving instance Monad (ParseEff s err es)

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

{-# INLINE runCatch #-}
runCatch :: (Throw e :> es, FOEs es) => Eff (Catch e ': es) ~> Eff es
runCatch = interpret handleCatch

{-# INLINE handleCatch #-}
handleCatch :: (Throw e :> es, FOEs es) => Catch e ~~> Eff es
handleCatch (Catch action hdl) = action & Hefty.interposeWith \(Throw e) _ -> hdl e

{-# INLINE inCatch #-}
inCatch :: (Throw e :> es, FOEs es) => ParseEff s e (Catch e : es) a -> (e -> ParseEff s e (Catch e : es) a) -> ParseEff s e es a
inCatch (ParseEff p) h = ParseEff $ runCatch $ catch p (asEff . h)

{-# INLINE throw #-}
throw :: (Throw err :> es) => err -> ParseEff s err es a
throw = ParseEff . Hefty.throw

{-# INLINE runThrow #-}
runThrow :: (FOEs es) => ParseEff s err (Throw err : es) a -> ParseEff s err es (Either err a)
runThrow = ParseEff . Hefty.runThrow . asEff


-- 下面的接口参考了 megaparsec 的接口设计，提供了一些基本的 parser combinator 接口
{-# INLINE[2] try #-}
try :: (ParseEffConstraints s err es) => ParseEff s err es a -> ParseEff s err es a
try p = do
    s <- getParserState
    inCatch (raise p) (\e -> do
                putParserState s
                throw e
            )

{-# INLINE compact1 #-}
compact1 :: forall e s es err. (e :> es) => ParseEff s err (e : es)  ~> ParseEff s err es
compact1 = ParseEff . Hefty.translate id . asEff

{-# INLINE[2] observing' #-}
observing' :: (FOEs es) => ParseEff s err (Throw err: es) a -> ParseEff s err es (Either err a)
observing' = runThrow

{-# INLINE[2] observing #-}
observing :: (ParseEffConstraints s err es) => ParseEff s err es a -> ParseEff s err es (Either err a)
observing = observing' . raise

{-# RULES 
    "try observing" forall p. try (observing p) = observing p
#-}

{-# INLINE[2] _empty #-}
_empty :: ParseEffConstraints s err es => ParseEff s err es a
_empty = compact1 empty'

{-# INLINE empty' #-}
empty' :: (IsString err) => ParseEff s err (Throw err: es) a
empty' = throw "empty"

{-# INLINE[2] _alter #-}
_alter :: ParseEffConstraints s err es => ParseEff s err es a -> ParseEff s err es a -> ParseEff s err es a
_alter p1  p2 = do
    inCatch (raise p1) (\_ -> raise p2)

many' :: (ParserST s :> es, FOEs es) => ParseEff s err (Throw err: es) a -> ParseEff s err es [a]
many' p = do
    s <- getParserState
    next <- observing' p
    case next of
        Left _ -> 
            putParserState s >>
            return []
        Right r -> (r :) <$> many' p

{-# INLINE[2] _many #-}
_many :: ParseEffConstraints s err es => ParseEff s err es a -> ParseEff s err es [a]
_many = many' . raise

instance (ParseEffConstraints s err es) => Alternative (ParseEff s err es) where
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
"observing empty" observing _empty = return (Left "empty")
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
(<|>+) :: forall s err a b es1 es2. 
            (KnownLength es1, ParseEffConstraints s err (es1 ++ es2))
        => ParseEff s err es1 a -> ParseEff s err es2 b -> ParseEff s err (es1 ++ es2) (Either a b)
(<|>+) (ParseEff p1) (ParseEff p2) = do
    inCatch 
        (raise $ ParseEff $ (Hefty.raiseSuffix @es2) (Left <$> p1)) 
        (\_ -> raise $ ParseEff $ (Hefty.raisePrefix @es1) (Right <$> p2))

{-# INLINE[2] lookAhead #-}
lookAhead :: (ParseEffConstraints s err es) => ParseEff s err es a -> ParseEff s err es (Maybe a)
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
satisfy :: (ParseEffConstraints s err es, S.Stream s, Show (S.Token s)) => (Maybe (S.Token s) -> Bool) -> ParseEff s err es (Maybe (S.Token s))
satisfy p = do
    s <- getParserState
    if p (S.head s) then do
        putParserState (S.tail s)
        return $ S.head s
    else
        throw $ fromString ("satisfy: unexpected token" <> show (S.head s))

{-# RULES 
    "satisfy_ or" forall p1 p2. satisfy_ p1 <|> satisfy_ p2 = satisfy_ (\t -> p1 t || p2 t) 
#-}


{-# INLINE[2] satisfy_ #-}
-- fail on EOF
satisfy_ :: (ParseEffConstraints s err es, S.Stream s, Show (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Token s)
satisfy_ p = do
    s <- getParserState
    case S.head s of
        Just t | p t -> do
            putParserState (S.tail s)
            return t
        _ -> throw $ fromString ("satisfy_: unexpected token" <> show (S.head s))

{-# RULES 
"asum satisfy_" forall pl. asum (map satisfy_ pl) = satisfy_ (\x -> any (x &) pl)
"asum satisfy" forall pl. asum (map satisfy pl) = satisfy (\x -> any (x &) pl)
#-}

{-# INLINE[2] eof #-}
eof :: (ParseEffConstraints s err es, S.Stream s, Show (S.Token s)) => ParseEff s err es ()
eof = do
    s <- getParserState
    if isNothing $ S.head s then return () else throw $ fromString ("eof: expected end of input, but got " <> show (S.head s))

{-# INLINE tokens #-}
tokens :: forall s err es. (ParseEffConstraints s err es, S.Stream s, S.TokensConstraints s) => S.Tokens s -> ParseEff s err es ()
tokens ts = do
    s <- getParserState
    let (heads, rest) = S.takeN_ (S.tokenLen @s ts) s
    if heads == ts then do
        putParserState rest
        return ()
    else
        throw $ fromString ("tokens: expected " <> show ts <> ", but got " <> show heads)

{-# RULES 
    "many satisfy" forall p. many (satisfy_ p) = takeWhileP' p
#-}
{-# RULES 
    "some satisfy" forall p. some (satisfy_ p) = takeWhileP1' p
#-}
{-# INLINE takeWhileP #-}
takeWhileP :: (ParseEffConstraints s err es, S.Stream s) => (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
takeWhileP p = do
    s <- getParserState
    let (ts, rest) = S.takeWhile_ p s
    putParserState rest
    return ts
{-# INLINE takeWhileP' #-}
takeWhileP' :: forall s err es. (ParseEffConstraints s err es, S.Stream s) => (S.Token s -> Bool) -> ParseEff s err es [S.Token s]
takeWhileP' p = fmap (S.toList @s) (takeWhileP p)

{-# INLINE takeWhileP1 #-}
takeWhileP1 :: (ParseEffConstraints s err es, S.Stream s, Show (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Tokens s)
takeWhileP1 p = satisfy_ p >> takeWhileP p

{-# INLINE takeWhileP1' #-}
takeWhileP1' :: forall s err es. (ParseEffConstraints s err es, S.Stream s, Show (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es [S.Token s]
takeWhileP1' p = fmap (S.toList @s) (takeWhileP1 p)


-- data Loc = Loc
--     { line :: Int
--     , column :: Int
--     } deriving (Show, Eq)

-- type Located a = (Loc, a)


-- deriving instance  Hefty.OrderOf (ParseEffE str pos)