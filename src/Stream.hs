module Stream where

-- 使用 vector 来实现一个 Stream 型结构
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Kind (Type)
import Data.Maybe (isNothing)

class Stream s where
    {-# INLINE head #-}
    {-# INLINE tail #-}
    {-# INLINE takeWhile_ #-}
    {-# INLINE takeN_ #-}
    type Token s :: Type
    uncons :: s -> Maybe (Token s, s)
    head :: s -> Maybe (Token s)
    head s = case uncons s of
        Just (t, _) -> Just t
        Nothing -> Nothing
    tail :: s -> s
    tail s = case uncons s of
        Just (_, rest) -> rest
        Nothing -> s
    takeWhile_ :: (Token s -> Bool) -> s -> (V.Vector (Token s), s)
    takeWhile_ p _s = case uncons _s of
            Just (t, rest) | p t -> let (ts, finalRest) = takeWhile_ p rest in (V.cons t ts, finalRest)
            _ -> (V.empty, _s)
    takeN_ :: Int -> s -> (V.Vector (Token s), s)
    takeN_ 0 s = (V.empty, s)
    takeN_ n s = case uncons s of
            Just (t, rest) -> let (ts, finalRest) = takeN_ (n - 1) rest in (V.cons t ts, finalRest)
            Nothing -> (V.empty, s)
    -- elemAt :: Int -> s -> Maybe (Token s)
    -- toVector :: s -> V.Vector (Token s)
    -- fromVector :: V.Vector (Token s) -> s
    -- fromList :: [Token s] -> s
    -- fromList = fromVector . V.fromList
    
type Tokens s = V.Vector (Token s) 

isEmpty :: Stream s => s -> Bool
isEmpty = isNothing . uncons 

instance Stream (V.Vector a) where
    {-# INLINE uncons #-}
    {-# INLINE takeWhile_ #-}
    {-# INLINE takeN_ #-}
    -- {-# INLINE elemAt #-}
    -- {-# INLINE toVector #-}
    -- {-# INLINE fromVector #-}
    type Token (V.Vector a) = a
    uncons = V.uncons
    takeWhile_ p v = let (t, r) = V.span p v in (t, r)
    takeN_ n v = let (t, r) = V.splitAt n v in (t, r)
    -- elemAt i v = if i < V.length v then Just (v V.! i) else Nothing
    -- toVector = id
    -- fromVector = id

instance Stream T.Text where
    {-# INLINE uncons #-}
    -- {-# INLINE elemAt #-}
    -- {-# INLINE toVector #-}
    -- {-# INLINE fromVector #-}
    -- {-# INLINE fromList #-}
    type Token T.Text = Char
    uncons = T.uncons
    -- elemAt i t = if i < T.length t then Just (T.index t i) else Nothing
    -- toVector = V.fromList . T.unpack
    -- fromVector = T.pack . V.toList
    -- fromList = T.pack


