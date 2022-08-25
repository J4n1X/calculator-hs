import Control.Applicative
import Text.Printf (printf)

import CalcParser
import CalcDefs
import CalcInterpreter

main :: IO ()
main = do
    case parseText "(2 2)" of
      Right tokens -> do
        print tokens
        print $ interpTokens tokens;
      Left error   -> print error
