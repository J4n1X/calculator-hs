import Control.Applicative
import Text.Printf (printf)

import CalcParser
import CalcDefs
import CalcInterpreter

main :: IO ()
main = do
    case parseText "2+3*5 - 2" of
      Right tokens -> do
        print tokens
        print $ interpNumber (InterpState 0 tokens)
        print $ interpOperation (interpExpr (InterpState 0 tokens)) 0
      Left error   -> print error
