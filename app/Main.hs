import Control.Applicative
import Text.Printf (printf)
import System.Environment

import CalcParser
import CalcDefs
import CalcInterpreter

import Control.Lens

-- parseArg :: [String] -> IO ()
-- parseArg (arg:rem) = do
--   case parseExpr "<argv>" arg of
--     Right expr -> do
--       putStrLn $ "Input Text: " ++ arg
--       print expr
--       putStrLn "This expression evaluates to:"
--       putStrLn $ maybe "Nothing" show (interpExpr expr)
--     Left error   -> print error
--   parseArg rem
-- parseArg [] = do
--   return ()
-- 
-- main :: IO ()
-- main = do
--   args <- getArgs
--   parseArg args

import Control.Monad.Trans
import System.Console.Haskeline
import Data.Char (toLower)
import Data.Either (fromRight)
import Text.Parsec (ParseError)

process :: String -> IO ()
process line = do
  let res = parseStmt "<stdin>" line
  case res of
    Left err -> print err
    Right st -> do
      putStrLn "AST is:"
      print st
      putStrLn "Interpreted result is:"
      putStrLn $ maybe "Nothing" show $ view interpCarry (interpStmt (InterpState [] [] $ Just 0) st)

checkResult res = case res of
  Left err -> error $ show err
  Right st -> st

processMany :: [String] -> Bool -> IO ()
processMany input verbose =
  let resState = interpTopLevel stmts in
    if verbose then
      printVerbose resState
    else
      printRegular resState
  where
    stmts = map (checkResult . parseStmt "<stdin>") input
    printRegular state = do
      putStrLn $ "Parsed " ++ show (length input) ++ " lines"
      putStrLn "The entered code evaluated to:"
      print $ view interpCarry state
    printVerbose state = do
      putStrLn "The Program AST is:"
      mapM_ print stmts
      
      putStrLn "\nThe Function List contains:"
      if not $ null $ view interpFunctions state then
        mapM_ print (view interpFunctions state)
      else
        putStrLn "Nothing"

      putStrLn "\nThe Variable List contains:"
      if not (null (view interpVariables state)) then
        mapM_ print (view interpVariables state)
      else
        putStrLn "Nothing"
      putStr "\n"
      printRegular state


main :: IO ()
main = runInputT defaultSettings $ loop []
  where
  loop prev = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> checkInput prev input
  checkInput prev input
    | map toLower input == "help"    =
      outputStrLn
        (    "Enter \"run\" to execute the code.\n"
          ++ "Enter \"runverbose\" to display interpreter state"
          ++ "Enter \"list\" to list the program"
          ++ "Enter \"clear\" to clear the code"
          ++ "Enter \"exit\" or \"quit\" to exit the application\n"
        ) >> loop prev
    | map toLower input == "run"        = liftIO (processMany prev False) >> loop prev
    | map toLower input == "runverbose" = liftIO (processMany prev True)  >> loop prev
    | map toLower input == "clear"      = loop []
    | map toLower input == "list"       = liftIO (putStrLn $ unlines prev) >> loop prev
    | map toLower input == "quit"
      || map toLower input == "exit"    = outputStrLn "Quitting..."
    | otherwise                         = loop $ prev ++ [input]