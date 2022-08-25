{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module CalcParser
  ( InputData (inputPos, inputText),
    parseText
  )
where

import CalcDefs
import Control.Applicative
import Data.Char


-- This file contains the parsing component of the Calculator

data InputData = InputData
  { inputPos :: Int,
    inputText :: String
  }
  deriving (Show)

inputStep :: InputData -> Maybe (Char, InputData)
inputStep (InputData _ []) = Nothing
inputStep (InputData pos (x : rem)) = Just (x, InputData (pos + 1) rem)

data ParserError = ParserError Int String deriving (Show)

-- NOTE: no proper error reporting
newtype Parser a = Parser
  { runParser :: InputData -> Either ParserError (InputData, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f input@(inputStep -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise =
        Left $
          ParserError (inputPos input) ("Expected '" ++ [x] ++ "' but found '" ++ [y] ++ "'")
    f input =
      Left $
        ParserError (inputPos input) ("Unexpected end of string while expecting '" ++ [x] ++ "'")

-- | Create a parser for a specific string
stringP ::
  String -> -- String to find in the input
  Parser String
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

parseIf ::
  String -> -- Description of the predicate
  (Char -> Bool) -> -- predicate
  Parser Char
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

-- Negative numbers should never parse, as we have operators that are parsed first
calcNumber :: Parser CalcToken
calcNumber =
  CalcNumber . read
    <$> ( (++)
            <$> ((:) <$> charP '-' <*> digits <|> digits)
            <*> (((:) <$> charP '.' <*> digits <|> digits) <|> pure [])
        )
  where
    digits = some $ parseIf "isDigit" isDigit

calcBlock :: Parser CalcToken
calcBlock = CalcBlock <$> (charP '(' *> ws *> elements <* ws <* charP ')')
  where
    elements = sepBy ws calcToken

calcOperator :: Parser CalcToken
calcOperator =
  CalcOperator
    <$> ( ('+' <$ charP '+')
            <|> ('-' <$ charP '-')
            <|> ('*' <$ charP '*')
            <|> ('/' <$ charP '/')
        )

calcToken :: Parser CalcToken
calcToken = calcOperator <|> calcNumber <|> calcBlock 

calcBody :: Parser [CalcToken]
calcBody = sepBy ws calcToken

-- Parse a given String
parseText :: String -> Either ParserError [CalcToken]
parseText text = do
  case runParser calcBody $ InputData 0 text of
    Left e -> Left e
    Right (_, x) -> Right x

-- Parse an entire file
parseFile :: FilePath -> Parser a -> IO (Either ParserError a)
parseFile fileName parser = do
  input <- readFile fileName
  case runParser parser $ InputData 0 input of
    Left e -> return $ Left e
    Right (_, x) -> return $ Right x
