module Printer (
    red,
    redUnderline,
    redBold,
    printErrorForest,
    printErrorForestE,
    Doc,
    SourceViewer,
    HasSourceViewer,
    pretty,
    getSourceLine
) where
import Data.Tree (Forest, Tree(..))
import qualified Data.Text as T
import qualified Prettyprinter as PP
import Prettyprinter((<+>), indent, line, vsep, annotate, pretty, space)
import qualified Prettyprinter.Render.Terminal as PP
import Utils (Position(..))
import Data.Vector (Vector, (!?))
import Fmt (padLeftF, Builder(..))
import Data.Text.Lazy.Builder (toLazyText)
import qualified Control.Monad.Hefty as Hefty
import Effect
import Control.Monad.Hefty.Reader (ask'_)

type Doc = PP.Doc PP.AnsiStyle
type SourceViewer = Hefty.Ask (Vector T.Text)
type HasSourceViewer es = SourceViewer `Hefty.In` es

getSourceLine :: (HasSourceViewer es) => Int -> ParseEff s err es (Maybe T.Text)
getSourceLine lineNum = do
    sources <- ParseEff ask'_
    return $ sources !? (lineNum - 1)

red :: PP.AnsiStyle
red = PP.color PP.Red

redUnderline :: PP.AnsiStyle
redUnderline = PP.color PP.Red <> PP.underlined

redBold :: PP.AnsiStyle
redBold = PP.color PP.Red <> PP.bold

nSpace :: Int -> Doc
nSpace n = pretty (replicate n ' ')

cutText :: Int -> Int -> T.Text -> (T.Text, T.Text, T.Text)
cutText start end text =
    let text1 = T.replace "\n" "\\n" text in
    let (pre, rest) = T.splitAt (start - 1) text1 in
    let (cur, post) = T.splitAt (end - start) rest in
    (pre, cur, post)

printSrc :: Vector T.Text -> (Position, Position) -> Doc
printSrc sourceCode (Position startLine startCol, Position endLine endCol) =
    let allLines = [startLine .. endLine] in
    let lineNumSpace = max (length (show startLine)) (length (show endLine)) in
    let wrapContent = nSpace lineNumSpace <> " | " in
    let codeLines = map (\lineNum ->
                case sourceCode !? (lineNum - 1) of
                    Just lineContent ->
                        let (pre, cur, post) = cutText startCol endCol lineContent in
                        (pretty . toLazyText) (padLeftF lineNumSpace ' ' lineNum) <> " | " <> pretty pre <> annotate redUnderline (pretty cur) <> pretty post
                    Nothing -> PP.emptyDoc
            ) allLines in
    PP.align $ vsep (wrapContent : codeLines ++ [wrapContent])

printErrorTree :: Vector T.Text -> Tree (Doc, (Position, Position)) -> Doc
printErrorTree sourceCode (Node (errMsg, (startPos, endPos)) subTrees) =
        let subErrors = printErrorForest sourceCode subTrees
            srcInfo = printSrc sourceCode (startPos, endPos)
        in  if null subTrees
            then
                vsep [srcInfo, errMsg]
            else
                vsep [srcInfo, errMsg <+> "at" <+> pretty (show endPos) <>  ":", indent 2 subErrors]

printErrorForest :: Vector T.Text -> Forest (Doc, (Position, Position)) -> Doc
printErrorForest _ [] = PP.emptyDoc
printErrorForest sourceCode [a] = printErrorTree sourceCode a
printErrorForest sourceCode forest = "All the following" <+> (length forest `PP.pretty`) <+> "alternatives " <> annotate red "failed " <> ":" <> PP.line <> indent 2 (vsep (map (printErrorTree sourceCode) forest))

printErrorForestE :: (HasSourceViewer es) => Forest (Doc, (Position, Position)) -> ParseEff s err es Doc
printErrorForestE forest = do
    sourceCode <- ParseEff ask'_
    return $ printErrorForest sourceCode forest


