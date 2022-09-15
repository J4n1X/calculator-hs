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
import Data.Maybe (fromMaybe)

importFile :: FilePath -> IO String
importFile path = do
  fileText <- readFile path
  return $ fileText

checkResult res = case res of
  Left err -> error $ show err
  Right st -> st

process :: InterpState -> String -> IO InterpState
process state input = do
  putStrLn $ fromMaybe "Nothing" (view interpCarry resState >>= (Just . show))
  return resState
  where
    resState = interpTopLevel state stmts
    stmts = (checkResult . parseTopLevel "<stdin>") input


printState :: InterpState -> IO ()
printState state = do
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

main :: IO ()
main = runInputT defaultSettings $ loop (InterpState [] [] (Just 0) 0) "" False
  where
  loop state buffer multi = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> checkInput state buffer input multi
  inputLower input = map toLower input 
  checkInput state buffer input multiLine
    | inputLower input == ":help"                         = outputStrLn
                                                              (    "Enter \":state\" to display the current interpreter state\n"
                                                                ++ "Enter \":reset\" to remove all interpreted code\n"
                                                                ++ "Enter \":import <filename>\" to import code\n"
                                                                ++ "Enter \":begin\" to begin multiline function\n"
                                                                ++ "Enter \":end\" to execute multiline function\n"
                                                                ++ "Enter \":clear\" to clear multiline buffer\n"
                                                                ++ "Enter \":exit\" or \":quit\" to exit the application"
                                                              ) >> loop state buffer multiLine
    | inputLower input                == ":state"         = liftIO (printState state) >> loop state buffer multiLine
    | inputLower input                == ":reset"         = loop (InterpState [] [] (Just 0) 0) "" False
    | head (words (inputLower input)) == ":import"        = do
                                                              importedLines <- liftIO (importFile $ last (words $ inputLower input))
                                                              newState <- liftIO (process state importedLines)
                                                              loop newState buffer multiLine
    | inputLower input == ":begin"                        = loop state "" True
    | inputLower input == ":end"                          = do
                                                              newState <- liftIO (process state input)
                                                              loop newState "" False
    | inputLower input == ":clear"                        = loop state "" multiLine
    |    inputLower input == ":quit"
      || inputLower input == ":exit"                      = outputStrLn "Quitting..."
    | head input == ':'                                   = outputStrLn "Invalid command, enter \":help\" for help." >> loop state buffer multiLine
    | multiLine                                           = loop state (buffer ++ input) multiLine
    | otherwise                                           = do
                                                              newState <- liftIO (process state input)
                                                              loop newState "" multiLine
