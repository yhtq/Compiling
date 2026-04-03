module Main (main) where
import Control.Monad.Hefty
import Control.Monad.Hefty.State

il :: [Int]
il = repeat 1

ilEa :: Eff '[State Int] [Int]
ilEa = do
    n <- get
    modify (+1)
    rest <- ilEa
    return (n:rest)

heads :: Eff '[State Int] [Int] -> Eff '[State Int] Int
heads eff = do
    xs <- eff
    return (head xs)

main :: IO ()
main = 
    let (_, il1) = runPure $ runState 0 ilEa
     in
    print $ take 10 il1