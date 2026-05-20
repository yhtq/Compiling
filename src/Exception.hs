module Exception where
import qualified Control.Monad.Hefty as Hefty
import Control.Monad.Hefty (
    FOEs,
    Eff,
    Effect,
    makeEffectF,
    type (:>),
 )
import qualified Data.Tree as T
import Control.Monad.Hefty.Reader as Hefty

-- 抛出异常的同时捕获一个当前状态作为输出
type StatefulThrow node s es = (MultiThrow (node, s) :> es, Ask s :> es)

data MultiThrow node :: Effect where
    RawThrow :: node -> MultiThrow node f a
    ConsThrow :: MultiThrow node f a -> MultiThrow node f b -> MultiThrow node f c
    TreeThrow :: node -> MultiThrow node f a -> MultiThrow node f b
makeEffectF ''MultiThrow

translateToTree :: MultiThrow node f a -> T.Forest node
translateToTree (RawThrow node) = [T.Node node []]
translateToTree (ConsThrow e1 e2) = translateToTree e1 ++ translateToTree e2
translateToTree (TreeThrow node e) = [T.Node node (translateToTree e)]

-- 如果两个分支都失败，则将两个异常合并成一个异常树
alter :: (MultiThrow node :> es, FOEs es) => Eff es a -> Eff es a -> Eff es a
alter action1 action2 = do
    Hefty.interposeWith (\(e1 :: MultiThrow node f a) _ ->
            Hefty.interposeWith (\(e2 :: MultiThrow node f b) _ ->
                Hefty.perform (ConsThrow e1 e2)
            ) action2
        ) action1 

-- 使用给定的 handler 捕获异常
catch :: (MultiThrow node :> es, FOEs es) => Eff es a -> (forall x. MultiThrow node (Eff es) x -> Eff es a) -> Eff es a
catch action handler = Hefty.interposeWith (\(e :: MultiThrow node (Eff es) _) _ -> handler e) action

inState :: (StatefulThrow node s es) => node -> Eff es (node, s)
inState node = do
    s <- Hefty.ask
    return (node, s)

-- 为了保证 throw 时能够访问到当前的状态，我们将一个自定义的状态和异常节点一起抛出
throw :: (StatefulThrow node s es) => node -> Eff es a
throw node = inState node >>= rawThrow

treeThrows :: (StatefulThrow node s es) => node -> MultiThrow (node, s) (Eff es) a -> Eff es a
treeThrows node e = do
    s <- Hefty.ask
    treeThrow (node, s) e

withInStack :: (MultiThrow node :> es, FOEs es) => node -> Eff es a -> Eff es a
withInStack node action = catch action $ \e -> Hefty.perform (TreeThrow node e)

{-# INLINE reThrow #-}
reThrow :: (MultiThrow node :> es) => MultiThrow node (Eff es) a -> Eff es b
reThrow (RawThrow node) = rawThrow node
reThrow (ConsThrow e1 e2) = consThrow e1 e2
reThrow (TreeThrow node e) = treeThrow node e

-- {-# INLINE runCatch #-}
-- runCatch :: forall e es. (Throw e :> es, FOEs es, Stack :> es, State [Int] :> es) => Eff (Catch e ': es) ~> Eff es
-- runCatch = interpret handleCatch 

-- appendCatchStack :: (State [Int] :> es) => Eff es ()
-- appendCatchStack = do
--     stack <- get
--     put (0:stack)

-- pushCatchStack :: (State [Int] :> es) => Eff es ()
-- pushCatchStack = do
--     stack <- get
--     case stack of
--         [] -> undefined -- should never happen
--         (x:xs) -> put ((x+1):xs)
-- popCatchStack :: (State [Int] :> es) => Eff es ()
-- popCatchStack = do
--     stack <- get
--     case stack of
--         [] -> undefined -- should never happen
--         (x:xs) -> put ((x-1):xs)
-- getCurCatchStack :: (State [Int] :> es) => Eff es Int
-- getCurCatchStack = do
--     stack <- get
--     case stack of
--         [] -> undefined -- should never happen
--         (x:_) -> return x
-- exitCatchStack :: (State [Int] :> es) => Eff es ()
-- exitCatchStack = do
--     stack <- get
--     case stack of
--         [] -> undefined -- should never happen
--         (_:xs) -> put xs

-- {-# INLINE handleCatch #-}
-- handleCatch :: (Throw e :> es, FOEs es, State [Int] :> es, Stack :> es) 
--     => Catch e ~~> Eff es
-- handleCatch (Catch action hdl) = 
--     appendCatchStack >>
--     action & 
--     Hefty.interpose (\case
--         PushStack -> pushCatchStack >> pushStack 
--         PopStack -> popCatchStack >> popStack
--     ) &
--     Hefty.interposeWith \(Throw e) _ -> do
--         curStack <- getCurCatchStack
--         replicateM_ curStack popCatchStack
--         hdl e