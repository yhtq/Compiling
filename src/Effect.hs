{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Effect where
import qualified Control.Monad.Hefty as Hefty
import qualified Stream as S
import qualified Control.Effect as CE
import Control.Monad.Hefty (
    type (++),
    raise,
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
import Control.Monad.Hefty.Except (catch, throw, throw', throw'_)
import Data.Effect.OpenUnion (KnownLength, union)
import qualified Control.Monad.Hefty.State as Hefty
import Data.String (IsString(fromString))
import Data.Maybe (isNothing)
import Control.Applicative (Alternative)

newtype ParserE s f a = ParserE (State s f a)
type instance Hefty.OrderOf (ParserE s) = Hefty.OrderOf (State s)
type instance Hefty.LabelOf (ParserE s) = Hefty.LabelOf (State s)
type instance Hefty.FormOf (ParserE s) = Hefty.FormOf (State s)
deriving instance Hefty.PolyHFunctor (ParserE s)
deriving instance Hefty.FirstOrder (ParserE s)

getParserState :: (ParserE s :> es) => Eff es s
getParserState = Hefty.perform (ParserE Hefty.Get)

putParserState :: (ParserE s :> es) => s -> Eff es ()
putParserState s = Hefty.perform (ParserE (Hefty.Put s))

type ParseEffConstraints s err es = (FOEs es, ParserE s :> es, Throw err :> es, IsString err)
type ParseEff s err es a = ParseEff (Eff es a)

runCatch :: (Throw e :> es, FOEs es) => Eff (Catch e ': es) ~> Eff es
runCatch = interpret handleCatch

handleCatch :: (Throw e :> es, FOEs es) => Catch e ~~> Eff es
handleCatch (Catch action hdl) = action & Hefty.interposeWith \(Throw e) _ -> hdl e

-- 下面的接口参考了 megaparsec 的接口设计，提供了一些基本的 parser combinator 接口

try :: (ParseEffConstraints s err es) => ParseEff s err es a -> ParseEff s err es a
try p = do
    s <- getParserState
    runCatch $ catch (raise p) (\e -> do
                putParserState s
                throw e
            )

observing :: (ParseEffConstraints s err es) => ParseEff s err es a -> ParseEff s err es (Either err a)
observing p = runCatch $ catch (raise (Right <$> p)) (return . Left)

many :: (ParseEffConstraints s err es) => ParseEff s err es a -> ParseEff s err es [a]
many p = do
    s <- getParserState
    next <- observing p
    case next of
        Left _ -> 
            putParserState s >>
            return []
        Right r -> (r :) <$> many p

some :: (ParseEffConstraints s err es) => ParseEff s err es a -> ParseEff s err es [a]
some p = do
    r <- p
    rs <- many p
    return (r:rs)


compact :: forall es. (KnownLength es) => Eff (es ++ es) ~> Eff es
compact = Hefty.interprets @es (
            CE.Eff . Hefty.liftFree 
        )

(<>+) :: forall s err a b es1 es2. 
            (KnownLength es1)
        => ParseEff s err es1 a -> ParseEff s err es2 b -> ParseEff s err (es1 ++ es2) (a, b)
(<>+) p1 p2 = do
    ar <- (Hefty.raiseSuffix @es2) p1
    br <- (Hefty.raisePrefix @es1) p2
    return (ar, br)

-- (<>) :: forall s err a b es. 
--             (KnownLength es)
--         => ParseEff s err es a -> ParseEff s err es b -> ParseEff s err es (a, b)
-- (<>) p1 p2 = compact $ p1 <>+ p2 


(<|>+) :: forall s err a b es1 es2. 
            (KnownLength es1, ParseEffConstraints s err (es1 ++ es2))
        => ParseEff s err es1 a -> ParseEff s err es2 b -> ParseEff s err (es1 ++ es2) (Either a b)
(<|>+) p1 p2 = do
    runCatch @err $ catch (raise $ (Hefty.raiseSuffix @es2) (Left <$> p1)) (\_ -> raise $ (Hefty.raisePrefix @es1) (Right <$> p2))

lookAhead :: (ParseEffConstraints s err es) => ParseEff s err es a -> ParseEff s err es (Maybe a)
lookAhead p = do
    s <- getParserState
    next <- observing p
    putParserState s
    case next of
        Left _ -> return Nothing
        Right r -> return (Just r)

(<|>) :: forall s err a b es. 
            (ParseEffConstraints s err es)
        => ParseEff s err es a -> ParseEff s err es b -> ParseEff s err es (Either a b)
(<|>) p1 p2 = do
    runCatch @err $ catch (raise $ Left <$> p1) (\_ -> raise $ Right <$> p2)

satisfy :: (ParseEffConstraints s err es, S.Stream s, Show (S.Token s)) => (Maybe (S.Token s) -> Bool) -> ParseEff s err es (Maybe (S.Token s))
satisfy p = do
    s <- getParserState
    if p (S.head s) then do
        putParserState (S.tail s)
        return $ S.head s
    else
        throw $ fromString ("satisfy: unexpected token" <> show (S.head s))

-- fail on EOF
satisfy_ :: (ParseEffConstraints s err es, S.Stream s, Show (S.Token s)) => (S.Token s -> Bool) -> ParseEff s err es (S.Token s)
satisfy_ p = do
    s <- getParserState
    case S.head s of
        Just t | p t -> do
            putParserState (S.tail s)
            return t
        _ -> throw $ fromString ("satisfy_: unexpected token" <> show (S.head s))

eof :: (ParseEffConstraints s err es, S.Stream s, Show (S.Token s)) => ParseEff s err es ()
eof = do
    s <- getParserState
    if isNothing $ S.head s then return () else throw $ fromString ("eof: expected end of input, but got " <> show (S.head s))

tokens :: (ParseEffConstraints s err es, S.Stream s, Show (S.Token s), Eq (S.Token s)) => S.Tokens s -> ParseEff s err es ()
tokens ts = do
    s <- getParserState
    let (heads, rest) = S.takeN_ (length ts) s
    if S.toVector heads == ts then do
        putParserState rest
        return ()
    else
        throw $ fromString ("tokens: expected " <> show ts <> ", but got " <> show (S.toVector heads))

-- (<|>) :: forall s err a b es. 
--             (KnownLength es, ParseEffConstraints s err es)
--         => ParseEff s err es a -> ParseEff s err es b -> ParseEff s err es (Either a b)
-- (<|>) p1 p2 = compact $ p1 <|>+ p2

-- data Loc = Loc
--     { line :: Int
--     , column :: Int
--     } deriving (Show, Eq)

-- type Located a = (Loc, a)


-- deriving instance  Hefty.OrderOf (ParseEffE str pos)