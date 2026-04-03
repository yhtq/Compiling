module Stream where

-- 使用 vector 来实现一个 Stream 型结构
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Kind (Type)
import Data.Maybe (isNothing, listToMaybe)
import Control.Monad.Hefty (Effect, (:>))
import qualified Control.Monad.Hefty as Hefty
import Data.Functor ((<&>))

class TokenClass s where
    type Token s :: Type
    type Tokens s :: Type
    tokenLen :: Tokens s -> Int
    fromList :: [Token s] -> Tokens s
    toList :: Tokens s -> [Token s]

-- buf 是某种暂存状态，可以用来回溯 Stream 的状态
data Stream s buf :: Effect where
    TakeWhile :: (Token s -> Bool) -> Stream s buf f (Tokens s)
    TakeN :: Int -> Stream s buf f (Tokens s)
    Revert :: buf -> Stream s buf f ()
    Current :: Stream s buf f buf
    -- Uncons :: s -> Stream s f (Maybe (Token s, s))

Hefty.makeEffectF ''Stream


{-# INLINE head #-}
head :: forall s buf es. (TokenClass s, Stream s buf :> es) => Hefty.Eff es (Maybe (Token s))
head = do
    r <- takeN 1
    return $ listToMaybe $ toList @s r


instance TokenClass T.Text where
    type Token T.Text = Char
    type Tokens T.Text = T.Text
    tokenLen = T.length
    fromList = T.pack
    toList = T.unpack

-- class (Functor m) => StreamM m s where
--     {-# INLINE head #-}
--     {-# INLINE tail #-}
--     type Token s :: Type
--     type Tokens s :: Type
--     tokenLen :: Tokens s -> Int
--     uncons :: s -> m (Maybe (Token s, s))
--     head :: s -> m (Maybe (Token s))
--     head s = fmap (fmap fst) (uncons s)
--     tail :: s -> m s
--     tail s = fmap (maybe s snd) (uncons s)
--     takeWhile_ :: (Token s -> Bool) -> s -> m (Tokens s, s)
--     takeN_ :: Int -> s -> m (Tokens s, s)
--     -- elemAt :: Int -> s -> Maybe (Token s)
--     -- toVector :: s -> V.Vector (Token s)
--     -- fromVector :: V.Vector (Token s) -> s
--     fromList :: [Token s] -> Tokens s
--     toList :: Tokens s -> [Token s]


-- isEmpty :: StreamM m s => s -> m Bool
-- isEmpty = fmap isNothing . uncons 

-- instance (Applicative m) => StreamM m (V.Vector a) where
--     {-# INLINE uncons #-}
--     {-# INLINE takeWhile_ #-}
--     {-# INLINE takeN_ #-}
--     {-# INLINE tokenLen #-}
--     {-# INLINE fromList #-}
--     {-# INLINE toList #-}
--     type Token (V.Vector a) = a
--     type Tokens (V.Vector a) = V.Vector a
--     tokenLen = V.length
--     uncons = pure . V.uncons
--     takeWhile_ p v = let (t, r) = V.span p v in pure (t, r)
--     takeN_ n v = let (t, r) = V.splitAt n v in pure (t, r)
--     fromList = V.fromList
--     toList = V.toList


-- instance (Applicative m) => StreamM m T.Text where
--     {-# INLINE uncons #-}
--     {-# INLINE takeWhile_ #-}
--     {-# INLINE takeN_ #-}
--     {-# INLINE tokenLen #-}
--     {-# INLINE fromList #-}
--     {-# INLINE toList #-}
--     type Token T.Text = Char
--     type Tokens T.Text = T.Text
--     tokenLen = T.length
--     uncons = pure . T.uncons
--     takeWhile_ f s = let (t, r) = T.span f s in pure (t, r)
--     takeN_ n s = let (t, r) = T.splitAt n s in pure (t, r)
--     fromList = T.pack
--     toList = T.unpack
 
