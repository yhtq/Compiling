module Text 
(
    IsText(..),
    TShow(..)
)
where
import Data.Text(Text)
import qualified Data.Text as T


class IsText a where
    fromText :: Text -> a
instance IsText Text where
    fromText = id
instance IsText String where
    fromText = T.unpack

class TShow a where
    tshow :: a -> Text
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