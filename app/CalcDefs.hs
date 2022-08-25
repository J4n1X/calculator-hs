module CalcDefs (
  CalcToken (
    CalcNumber,
    CalcOperator,
    CalcBlock
  ),
  isOp,
  opPrec
) where

-- This file contains important structures and functions for the base of the calculator

data CalcToken
  = CalcNumber Double
  | CalcOperator Char
  | CalcBlock [CalcToken]
  deriving (Show, Eq)

isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp _   = False

opPrec :: Maybe Char -> Int
opPrec op = case op of
  Just '+' -> 10
  Just '-' -> 10 
  Just '*' -> 20
  Just '/' -> 20
  Just _   -> -1
  Nothing  -> -1