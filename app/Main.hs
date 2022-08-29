import Control.Applicative
import Text.Printf (printf)
import System.Environment

import CalcParser
import CalcDefs
import CalcInterpreter

parseArg :: [String] -> IO ()
parseArg (arg:rem) = do
  case parseText arg of
    Right tokens -> do
      print tokens
      print $ interpOperation (interpExpr (InterpState 0 tokens)) 0
    Left error   -> print error
  parseArg rem
parseArg [] = do
  return ()

main :: IO ()
main = do
  args <- getArgs
  parseArg args
