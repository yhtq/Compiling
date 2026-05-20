module Main (main) where

-- data HigherEmbIO z a where
--   Embed' :: IO a -> HigherEmbIO z a
--   PrefixSem :: String -> z a -> HigherEmbIO z a

-- makeSem ''HigherEmbIO


-- prefixIO :: String -> IO a -> IO a
-- prefixIO label = (putStrLn label >>)

-- runHigherEmbIO :: (Member (Final IO) r) => Sem (HigherEmbIO ': r) a -> Sem r a
-- runHigherEmbIO = interpretFinal $ \case
--   Embed' io -> liftS io
--   PrefixSem label sem -> do
--     p <- runS sem
--     pure $ prefixIO label p



-- bar, baz :: IO ()
-- bar = putStrLn "bar"
-- baz = putStrLn "baz"

-- main :: IO ()
-- main = do
--   prefixIO "FOO" $ bar >> baz
--   runFinal $ runHigherEmbIO $ prefixSem "FOO" $ embed' bar >> embed' baz

main = putStrLn "Hello, World!"