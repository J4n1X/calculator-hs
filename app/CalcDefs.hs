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

opPrec :: Char -> Int
opPrec '+' = 10
opPrec '-' = 10 
opPrec '*' = 20
opPrec '/' = 20
opPrec _   = -1