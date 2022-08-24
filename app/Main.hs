

import Control.Applicative
import Text.Printf (printf)

import CalcParser
import CalcDefs

main :: IO ()
main = do
    case parseText "(2 2)" of
      Right tokens -> do
        print tokens
        print (runInterpreter interpCalcBlock (InterpData 0 tokens));
      Left error   -> print error
