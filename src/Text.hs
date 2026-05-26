module Text
(
    IsText(..),
    TShow(..),
    FreshVarGen(..),
    runFreshVarGen,
    runFreshVarGen',
    genNew,
    gainNew,
    genNew',
    gainNew',
    genNew'',
    gainNew'',
    genNew'_,
    gainNew'_
)
where
import Data.Text(Text)
import Data.HashSet (Set, insert, member)
import qualified Data.Text as T
import Control.Monad.Hefty (Effect)
import qualified Control.Monad.Hefty as Hefty
import qualified Control.Monad.Hefty.State as Hefty
import Data.Hashable (Hashable)


class IsText a where
    fromText :: Text -> a
instance IsText Text where
    fromText = id
instance IsText String where
    fromText = T.unpack

class TShow a where
    tshow :: a -> Text
instance TShow Char where
    tshow = T.singleton
instance TShow Text where
    tshow = id
instance TShow String where
    tshow = T.pack
instance TShow () where
    tshow _ = "()"
instance (TShow a, TShow b) => TShow (a, b) where
    tshow (a, b) = "(" <> tshow a <> ", " <> tshow b <> ")"
instance (TShow a) => TShow [a] where
    tshow xs = "[" <> T.intercalate ", " (map tshow xs) <> "]"
instance (TShow a) => TShow (Maybe a) where
    tshow Nothing = "Nothing"
    tshow (Just x) = "Just " <> tshow x
instance TShow Int where
    tshow = T.pack . show

data FreshVarGen t :: Effect where
    GenNew :: FreshVarGen t f t
    GainNew :: t -> FreshVarGen t f ()
Hefty.makeEffectF ''FreshVarGen

runFreshVarGen :: (Hashable t, Ord t, IsText t) => Hefty.Eff (FreshVarGen t : es) a -> Hefty.Eff (Hefty.State (Int, Set t) : es) a
runFreshVarGen = Hefty.reinterpret $ \case
    GainNew var -> do
        (counter, symbolTable) <- Hefty.get
        if var `member` symbolTable then
            return () -- already exists, do nothing
        else
            Hefty.put (counter, var `insert` symbolTable) -- add to symbol table
    GenNew -> do
        let go = do
                (counter, symbolTable) <- Hefty.get
                let newVar = fromText ("a" <> tshow counter)
                if newVar `member` symbolTable then do
                    Hefty.put (counter + 1, symbolTable)
                    go
                else do
                    Hefty.put (counter + 1, newVar `insert` symbolTable)
                    return newVar
        go
runFreshVarGen' ::  (Hefty.FOEs es, Hashable t, Ord t, IsText t) => Hefty.Eff (FreshVarGen t : es) a -> Hefty.Eff es a
runFreshVarGen' = fmap snd . Hefty.runState (0, mempty) . runFreshVarGen
