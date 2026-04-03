module Stream where

-- 使用 vector 来实现一个 Stream 型结构
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Kind (Type)
import Data.Maybe (isNothing)

class Stream s where
    {-# INLINE head #-}
    {-# INLINE tail #-}
    type Token s :: Type
    type Tokens s :: Type
    tokenLen :: Tokens s -> Int
    uncons :: s -> Maybe (Token s, s)
    head :: s -> Maybe (Token s)
    head s = case uncons s of
        Just (t, _) -> Just t
        Nothing -> Nothing
    tail :: s -> s
    tail s = case uncons s of
        Just (_, rest) -> rest
        Nothing -> s
    takeWhile_ :: (Token s -> Bool) -> s -> (Tokens s, s)
    takeN_ :: Int -> s -> (Tokens s, s)
    -- elemAt :: Int -> s -> Maybe (Token s)
    -- toVector :: s -> V.Vector (Token s)
    -- fromVector :: V.Vector (Token s) -> s
    fromList :: [Token s] -> Tokens s
    toList :: Tokens s -> [Token s]


isEmpty :: Stream s => s -> Bool
isEmpty = isNothing . uncons 

instance Stream (V.Vector a) where
    {-# INLINE uncons #-}
    {-# INLINE takeWhile_ #-}
    {-# INLINE takeN_ #-}
    {-# INLINE tokenLen #-}
    {-# INLINE fromList #-}
    {-# INLINE toList #-}
    type Token (V.Vector a) = a
    type Tokens (V.Vector a) = V.Vector a
    tokenLen = V.length
    uncons = V.uncons
    takeWhile_ p v = let (t, r) = V.span p v in (t, r)
    takeN_ n v = let (t, r) = V.splitAt n v in (t, r)
    fromList = V.fromList
    toList = V.toList


instance Stream T.Text where
    {-# INLINE uncons #-}
    {-# INLINE takeWhile_ #-}
    {-# INLINE takeN_ #-}
    {-# INLINE tokenLen #-}
    {-# INLINE fromList #-}
    {-# INLINE toList #-}
    type Token T.Text = Char
    type Tokens T.Text = T.Text
    tokenLen = T.length
    uncons = T.uncons
    takeWhile_ = T.span
    takeN_ = T.splitAt
    fromList = T.pack
    toList = T.unpack


