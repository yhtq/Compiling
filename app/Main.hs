module Main (main) where

import qualified System.Environment as Env
import qualified System.IO as IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as Map
import Ast
import Parser
import Lexer
import Effect
import Utils hiding (fromText)
import TypeChecker
import Interpreter
import Printer
import qualified Control.Monad.Hefty as Hefty
import qualified Control.Monad.Hefty.Reader as Hefty
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP
import qualified Data.Tree as Tree
import Exception
import Text (TShow(..))

--------------------------------------------------------------------------------
-- Top-level utility: expose eraseTy from tests
--------------------------------------------------------------------------------

-- | Erase TyVar annotations for the type checker.
eraseTy :: PTerm -> PartialTypedTerm TypeLitO () LitO TrmVar
eraseTy (VarT v t)       = VarT v (eraseTyM t)
eraseTy (LitT l t)       = LitT l (eraseTyM t)
eraseTy (LamT v vt b bt) = LamT v (eraseTyM vt) (eraseTy b) (eraseTyM bt)
eraseTy (AppT f x t)     = AppT (eraseTy f) (eraseTy x) (eraseTyM t)
eraseTy (OLetT v vt b1 b2 t) = OLetT v (eraseTyM vt) (eraseTy b1) (eraseTy b2) (eraseTyM t)
eraseTy (RLetT v vt b1 b2 t) = RLetT v (eraseTyM vt) (eraseTy b1) (eraseTy b2) (eraseTyM t)

eraseTyM :: Maybe (Typ TypeLitO TyVar) -> Maybe (Typ TypeLitO ())
eraseTyM = fmap (fmap (const ()))

--------------------------------------------------------------------------------
-- Build expression from top-level bindings
--------------------------------------------------------------------------------

-- | Convert a list of top-level items into a single expression that
--   evaluates to the value of `main`.
--   Each binding becomes a let / let rec, wrapping around the body (the
--   next binding or the final `main` reference).
progToExpr :: [TopLevel TyVar TrmVar] -> Either Text PTerm
progToExpr toplevels = do
    let binds = [b | TLBind b <- toplevels]
    -- Ensure there is a `main` binding
    case filter isMainBind binds of
        [] -> Left "No binding for `main` found. The program must define `main`."
        _  -> Right $ buildExpr binds
  where
    isMainBind (Bind (TrmVar "main") _) = True
    isMainBind (RecBind (TrmVar "main") _) = True
    isMainBind _ = False

-- | Recursively wrap bindings into nested let / let rec expressions.
--   The body is a reference to `main` (so we extract its value).
buildExpr :: [Bind TyVar TrmVar] -> PTerm
buildExpr []     = varTerm (TrmVar "main" :: TrmVar)
buildExpr (b : rest) = case b of
    Bind v e    -> OLetT v Nothing e (buildExpr rest) Nothing
    RecBind v e -> RLetT v Nothing e (buildExpr rest) Nothing

--------------------------------------------------------------------------------
-- Type checking helper
--------------------------------------------------------------------------------

runTC :: PartialTypedTerm TypeLitO () LitO TrmVar
      -> Either (Tree.Forest TypeError) (TypedTerm (Typ TypeLitO ()) LitO TrmVar)
runTC term =
    let initCtx = Map.fromList
            [(TrmVar name, typ) | (name, typ) <- builtinPrimitiveTypes]
        p :: Hefty.Eff (TyEff TrmVar) (TypedTerm (Typ TypeLitO ()) LitO TrmVar)
        p = asEff (typeInfer term)
    in Hefty.runPure $ handleMultiThrow $ Hefty.runReader initCtx p

type TyEff v = '[Hefty.Local (TypingCtx v ()), Hefty.Ask (TypingCtx v ()), MultiThrow TypeError]

handleMultiThrow :: Hefty.Eff '[MultiThrow TypeError] a
                 -> Hefty.Eff '[] (Either (Tree.Forest TypeError) a)
handleMultiThrow = Hefty.interpretBy (pure . Right) (\e _k -> pure $ Left (translateToTree e))

--------------------------------------------------------------------------------
-- Main compiler pipeline
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- Env.getArgs
    case args of
        [file] -> runCompiler file
        _      -> IO.hPutStrLn IO.stderr "Usage: Compiling-exe <source-file>"

runCompiler :: FilePath -> IO ()
runCompiler file = do
    src <- T.readFile file
    case compile src of
        Left err -> T.hPutStrLn IO.stderr err
        Right result -> T.putStrLn result

compile :: Text -> Either Text Text
compile src = do
    -- Step 1: Lex and parse the source into a program
    (_, parsed) <- case simpleParse @[TopLevel TyVar TrmVar] src parseProg of
        (_, Left errDoc) -> Left $ T.pack (PP.renderString (PP.layoutPretty PP.defaultLayoutOptions errDoc))
        (_, Right prog)  -> Right ((), prog)

    -- Step 2: Convert top-level bindings into a let-expression
    expr <- progToExpr parsed

    -- Step 3: Erase type variables and run type checking
    typedTerm <- case runTC (eraseTy expr) of
        Left errs -> Left $ T.pack ("Type error:\n" ++ show errs)
        Right t   -> Right t

    -- Step 4: Verify that the result is of type String
    case typOfTTerm typedTerm of
        TLit TString -> return ()
        t            -> Left $ "`main` must have type `String`, but got: " <> tshow t

    -- Step 5: Evaluate
    case runEval typedTerm of
        Left errs        -> Left $ T.pack ("Runtime error:\n" ++ show errs)
        Right (VString s) -> Right s
        Right v          -> Left $ "Expected `main` to return a `String`, but got: " <> T.pack (show v)

--------------------------------------------------------------------------------
-- Re-exports for convenient use as a library
--------------------------------------------------------------------------------

runPipeline :: Text -> Either Text Text
runPipeline = compile
