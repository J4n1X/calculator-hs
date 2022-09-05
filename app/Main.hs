import Control.Applicative
import Text.Printf (printf)
import System.Environment

import CalcParser
import CalcDefs
import CalcInterpreter

parseArg :: [String] -> IO ()
parseArg (arg:rem) = do
  case parseExpr "<argv>" arg of
    Right expr -> do
      print expr
      print "This expression evaluates to:"
      print $ maybe "Nothing" show (interpExpr expr)
    Left error   -> print error
  parseArg rem
parseArg [] = do
  return ()

main :: IO ()
main = do
  args <- getArgs
  parseArg args
