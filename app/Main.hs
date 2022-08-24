{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Char
import Control.Applicative
import Text.Printf (printf)
import Control.Exception

data CalcToken
  = CalcNumber Double
  | CalcOperator Char
  | CalcBlock [CalcToken]
  deriving (Show, Eq)

data InputData = 
  InputData {
    inputPos  :: Int,
    inputText :: String
  } deriving (Show)

inputStep :: InputData -> Maybe (Char, InputData)
inputStep (InputData _ []) = Nothing
inputStep (InputData pos (x:rem)) = Just(x, InputData (pos+1) rem)


data ParserError = ParserError Int String deriving (Show)

-- NOTE: no proper error reporting
newtype Parser a = Parser
  { runParser :: InputData -> Either ParserError (InputData, a)
  }

data InterpData = InterpData {
  interpPos  :: Int,
  interpToks :: [CalcToken]
} deriving (Show)

newtype InterpError = InterpError String deriving (Show)

newtype Interpreter a = Interpreter
  { runInterpreter:: InterpData -> Either InterpError (InterpData, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Functor Interpreter where
  fmap f (Interpreter s)  =
    Interpreter $ \input -> do
      (input', x) <- s input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Applicative Interpreter where
  pure x = Interpreter $ \input -> Right (input, x)
  (Interpreter i1) <*> (Interpreter i2) = 
    Interpreter $ \input -> do
      (input', f) <- i1 input
      (input'', a) <- i2 input'
      return (input'', f a)


instance Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Alternative (Either InterpError) where
  empty = Left $ InterpError "empty"
  Left _ <|> e1 = e1
  e1     <|> _  = e1

instance Alternative Interpreter where 
  empty = Interpreter $ const empty
  (Interpreter i1) <|> (Interpreter i2) =
    Interpreter $ \input -> i1 input <|> i2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f input@(inputStep -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise = Left $ 
         ParserError (inputPos input) ("Expected '" ++ [x] ++ "' but found '" ++ [y] ++ "'")
    f input = Left $
      ParserError (inputPos input) ("Unexpected end of string while expecting '" ++ [x] ++ "'")

-- | Create a parser for a specific string
stringP :: String         -- String to find in the input
        -> Parser String
stringP str =
  Parser $ \input ->
    case runParser (traverse charP str) input of
      Left _ ->
        Left $
        ParserError
          (inputPos input)
          ("Expected \"" ++ str ++ "\", but found \"" ++ inputText input ++ "\"")
      result -> result

spanCondP :: String -> (Char -> Bool) -> Parser String
spanCondP desc = many . parseIf desc

parseIf :: String         -- Description of the predicate
        -> (Char -> Bool) -- predicate
        -> Parser Char
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputStep -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
          Left $
          ParserError
            (inputPos input)
            ("Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      _ ->
        Left $
        ParserError
          (inputPos input)
          ("Expected " ++ desc ++ ", but reached end of string")

-- NOTE: no escape support
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanCondP "isQuote" (/= '"') <* charP '"'

ws :: Parser String
ws = spanCondP "isSpace" isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp _   = False

opPrec :: Char -> Int
opPrec '+' = 10
opPrec '-' = 10 
opPrec '*' = 20
opPrec '/' = 20
opPrec _   = -1

calcNumber :: Parser CalcToken
calcNumber = CalcNumber . read 
  <$> ((++) 
  <$> ((:) <$> charP '-' <*> digits <|> digits) 
  <*> (((:) <$> charP '.' <*> digits <|> digits) <|> pure []))
  where 
    digits = some $ parseIf "isDigit" isDigit     

calcBlock :: Parser CalcToken
calcBlock = CalcBlock <$> (charP '(' *> ws *> elements <* ws <* charP ')' )
  where elements = sepBy ws calcToken

calcOperator :: Parser CalcToken
calcOperator = CalcOperator <$> (
  ('+' <$ charP '+') <|>
  ('-' <$ charP '-') <|>
  ('*' <$ charP '*') <|>
  ('/' <$ charP '/'))

calcToken :: Parser CalcToken
calcToken = calcNumber <|> calcBlock <|> calcOperator

calcBody :: Parser [CalcToken]
calcBody = sepBy ws calcToken

-- Parse a given String
parseText :: String -> Either ParserError [CalcToken]
parseText text = do
  case runParser calcBody  $ InputData 0 text of
    Left e       -> Left e
    Right (_,x) -> Right x 

-- Parse an entire file
parseFile :: FilePath -> Parser a -> IO (Either ParserError a)
parseFile fileName parser = do
  input <- readFile fileName
  case runParser parser $ InputData 0 input of
    Left e       -> return $ Left e
    Right (_, x) -> return $ Right x


nextToken :: InterpData -> Maybe (CalcToken, InterpData)
nextToken (InterpData _ [])       = Nothing
nextToken (InterpData pos (x:rem)) = Just(x, InterpData (pos+1) rem)

interpCalcNumber :: Interpreter Double
interpCalcNumber = Interpreter $ \input ->
  case input of
    (InterpData _ ((CalcNumber x):xs)) -> Right (InterpData (interpPos input + 1) xs, x)
    (InterpData _ (x:xs))              -> Left $ InterpError ("Expected Number, but got '" ++ show x  ++ "'instead")
    (InterpData _ [])                  -> Left $ InterpError "Expected Number, but got empty array"
    
-- create an entirely different interpreted instance
interpCalcBlock :: Interpreter Double
interpCalcBlock = Interpreter $ \input -> f input
  where 
    f input@(nextToken -> Just(tok, rem)) = 
        case tok of
          (CalcBlock block) -> do
            case runInterpreter interpExpr $ InterpData (interpPos rem) block of
              Right (rem', res) -> Right (InterpData (interpPos rem') (interpToks rem), res)
              Left e            -> Left e
          _ -> Left $ InterpError $ printf ("Expected a block, but got '" ++ show tok ++ "' instead at %d\n") (interpPos rem)
    f input = Left $ InterpError (printf "Failed to parse block at token %d\n" $ interpPos input)

interpCalcBinOp :: Double                 -- ^ Left Hand Side Value
                   -> Int                 -- ^ Previous Precedence
                   -> Interpreter Double  -- ^ Resulting Interpreter
interpCalcBinOp = undefined

{-- interpCalcNumber (CalcNumber num) = Right num
interpCalcNumber val = Left $
  InterpError ("Expected Number, but got '" ++ show val ++ "' instead")
--}

interpExpr :: Interpreter Double
interpExpr = interpCalcNumber <|> interpCalcBlock
  
interpText :: String -> Either InterpError Double
interpText text = case parseText text of
  Right tokens -> do
    (_ ,res) <- runInterpreter interpCalcBlock $ InterpData 0 tokens
    return res
  Left e       -> undefined


main :: IO ()
main = do
    case parseText "(2 2)" of
      Right tokens -> do
        print tokens
        print (runInterpreter interpCalcBlock (InterpData 0 tokens));
      Left error   -> print error
