module Lexer where
import qualified Stream as S
import Effect


digits :: forall s err es. (S.Stream s, S.Token s ~ Char, ?base :: Int) => ParseEff s err es [Char]
digits = do
    let isDigit c = case ?base of
            2 -> c == '0' || c == '1'
            8 -> c >= '0' && c <= '7'
            10 -> c >= '0' && c <= '9'
            16 -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
            _ -> False
    ds <- some @err (satisfy_ @s  isDigit)
    return ds