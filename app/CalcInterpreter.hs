{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module CalcInterpreter(
  InterpData( InterpData),
  Interpreter(runInterpreter),
  interpTokens
) where

import Control.Applicative
import Text.Printf (printf)
import CalcDefs
import CalcParser

-- This file contains the interpreting functions for the Calculator

data InterpData = InterpData {
  interpPos  :: Int,
  interpToks :: [CalcToken]
} deriving (Show)

newtype InterpError = InterpError String deriving (Show)

newtype Interpreter a = Interpreter
  { runInterpreter:: InterpData -> Either InterpError (InterpData, a)
  }

instance Functor Interpreter where
  fmap f (Interpreter s)  =
    Interpreter $ \input -> do
      (input', x) <- s input
      return (input', f x)

instance Applicative Interpreter where
  pure x = Interpreter $ \input -> Right (input, x)
  (Interpreter i1) <*> (Interpreter i2) = 
    Interpreter $ \input -> do
      (input', f) <- i1 input
      (input'', a) <- i2 input'
      return (input'', f a)

instance Alternative (Either InterpError) where
  empty = Left $ InterpError "empty"
  Left _ <|> e1 = e1
  e1     <|> _  = e1

instance Alternative Interpreter where 
  empty = Interpreter $ const empty
  (Interpreter i1) <|> (Interpreter i2) =
    Interpreter $ \input -> i1 input <|> i2 input

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

interpExpr :: Interpreter Double
interpExpr = interpCalcNumber <|> interpCalcBlock



  
interpTokens :: [CalcToken] -> Either InterpError Double
interpTokens tokens =
  case runInterpreter interpCalcBlock $ InterpData 0 tokens of
    Left e -> Left e
    Right (_, res) -> Right res
  
