module Stream where

-- 使用 vector 来实现一个 Stream 型结构
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Kind (Type)

class Stream s where
    {-# INLINE fromList #-}
    type Token s :: Type
    head :: s -> Maybe (Token s)
    tail :: s -> s
    takeWhile_ :: (Token s -> Bool) -> s -> (s, s)
    takeN_ :: Int -> s -> (s, s)
    elemAt :: Int -> s -> Maybe (Token s)
    toVector :: s -> V.Vector (Token s)
    fromVector :: V.Vector (Token s) -> s
    fromList :: [Token s] -> s
    fromList = fromVector . V.fromList
    
type Tokens s = V.Vector (Token s) 

instance Stream (V.Vector a) where
    {-# INLINE head #-}
    {-# INLINE tail #-}
    {-# INLINE takeWhile_ #-}
    {-# INLINE takeN_ #-}
    {-# INLINE elemAt #-}
    {-# INLINE toVector #-}
    {-# INLINE fromVector #-}
    type Token (V.Vector a) = a
    head v = case V.null v of
        True -> Nothing
        False -> Just (V.head v)
    tail v = case V.null v of
        True -> V.empty
        False -> V.tail v
    takeWhile_ p v = let (t, r) = V.span p v in (t, r)
    takeN_ n v = let (t, r) = V.splitAt n v in (t, r)
    elemAt i v = if i < V.length v then Just (v V.! i) else Nothing
    toVector = id
    fromVector = id

instance Stream T.Text where
    {-# INLINE head #-}
    {-# INLINE tail #-}
    {-# INLINE takeWhile_ #-}
    {-# INLINE takeN_ #-}
    {-# INLINE elemAt #-}
    {-# INLINE toVector #-}
    {-# INLINE fromVector #-}
    {-# INLINE fromList #-}
    type Token T.Text = Char
    head t = case T.null t of
        True -> Nothing
        False -> Just (T.head t)
    tail t = case T.null t of
        True -> T.empty
        False -> T.tail t
    takeWhile_ p t = let (t', r) = T.span p t in (t', r)
    takeN_ n t = let (t', r) = T.splitAt n t in (t', r)
    elemAt i t = if i < T.length t then Just (T.index t i) else Nothing
    toVector = V.fromList . T.unpack
    fromVector = T.pack . V.toList
    fromList = T.pack


