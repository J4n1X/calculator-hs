{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module CalcInterpreter
  ( InterpData (..),
    InterpState (..),
    interpNumber,
    interpOperation,
    mapStateCarry,
    interpExpr,
  )
where

import CalcDefs
import CalcParser
import Control.Applicative
import Data.Bifunctor (Bifunctor)
import GHC.Generics
import Text.Printf (printf)

-- This file contains the interpreting functions for the Calculator

data InterpData = InterpData
  { interpPos :: Int,
    interpToks :: [CalcToken]
  }
  deriving (Show)

data InterpState a b = InterpState
  { stateCarry :: a,
    stateTokens :: [b]
  }
  deriving (Show, Generic)

newtype InterpError = InterpError String deriving (Show)

-- newtype Interpreter a = Interpreter
--   { runInterpreter:: InterpData -> Either InterpError (InterpData, a)
--   }

-- instance Functor Interpreter where
--   fmap f (Interpreter s)  =
--     Interpreter $ \input -> do
--       (input', x) <- s input
--       return (input', f x)

-- instance Applicative Interpreter where
--   pure x = Interpreter $ \input -> Right (input, x)
--   (Interpreter i1) <*> (Interpreter i2) =
--     Interpreter $ \input -> do
--       (input', f) <- i1 input
--       (input'', a) <- i2 input'
--       return (input'', f a)

-- instance Alternative (Either InterpError) where
--   empty = Left $ InterpError "empty"
--   Left _ <|> e1 = e1
--   e1     <|> _  = e1

-- instance Alternative Interpreter where
--   empty = Interpreter $ const empty
--   (Interpreter i1) <|> (Interpreter i2) =
--     Interpreter $ \input -> i1 input <|> i2 input

carryOp :: Fractional a => Char -> (a -> a -> a)
carryOp '+' = (+)
carryOp '-' = (-)
carryOp '*' = (*)
carryOp '/' = (/)
carryOp _ = error "Invalid Operation"

nextStateToken :: InterpState a b -> Maybe (InterpState a b, b)
nextStateToken (InterpState carry state@(tok : rem)) = Just (InterpState carry rem, tok)
nextStateToken (InterpState _ []) = Nothing

setStateCarry :: InterpState a b -> a -> InterpState a b
setStateCarry state val = InterpState val (stateTokens state)

splitState :: InterpState Double b -> InterpState Double b
splitState (InterpState _ tokens) = InterpState 0.0 tokens

mapStateCarry :: Fractional a => Char -> InterpState a b -> InterpState a b -> InterpState a b
mapStateCarry op (InterpState carryA _) stateB@(InterpState carryB _) =
  setStateCarry stateB $ carryOp op carryA carryB

interpNumber :: InterpState Double CalcToken -> Maybe (InterpState Double CalcToken)
interpNumber state = case nextStateToken state of
  Just (rem, CalcNumber tok) -> Just $ setStateCarry rem tok
  Just (_, tok) -> Nothing
  Nothing -> Nothing

interpBlock :: InterpState Double CalcToken -> Maybe (InterpState Double CalcToken)
interpBlock state =
  case nextStateToken state of
    Just (rem, CalcBlock tok) -> Just $ setStateCarry rem $ stateCarry $ interpOperation (interpExpr $ InterpState 0 tok) 0
    Just (_, tok) -> Nothing
    Nothing -> Nothing

interpExpr :: InterpState Double CalcToken -> InterpState Double CalcToken
interpExpr state =
  case interpBlock state <|> interpNumber state of
    Just ret -> ret
    Nothing -> error ("Block parsing failed! State dump: \n" ++ show state)

interpOperation :: InterpState Double CalcToken -> Int -> InterpState Double CalcToken
interpOperation state@(InterpState _ []) _ = state
interpOperation state prevPrec = case nextOp state of
  (firstOp, state')
    | opPrec firstOp > prevPrec ->  interpNext state' prevPrec firstOp
    | otherwise -> interpOperation (state') (opPrec firstOp)
  where
    nextOp :: InterpState Double CalcToken -> (Maybe Char, InterpState Double CalcToken)
    nextOp state = case nextStateToken state of
      Just (opState, CalcOperator op) -> (Just op, opState)
      Just (_, _) -> (Nothing, state)
      Nothing -> (Nothing, state)
    interpNext :: InterpState Double CalcToken -> Int -> Maybe Char -> InterpState Double CalcToken
    interpNext state prevOp op
      | prevPrec >= opPrec op = interpOperation state (opPrec op)
      | prevPrec < opPrec op = calcStates state op $ interpOperation (interpExpr state) (opPrec op)
      | otherwise = error "Unexpected situation"
    calcStates :: InterpState Double CalcToken -> Maybe Char -> InterpState Double CalcToken -> InterpState Double CalcToken
    calcStates s1 (Just op) s2 = mapStateCarry op s1 s2
    calcStates s1 Nothing _ = s1