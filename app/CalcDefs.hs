{-# LANGUAGE GADTs #-}

module CalcDefs where

-- This file contains important structures and functions for the base of the calculator

data CalcToken
  = CalcNumber Double
  | CalcOperator Char
  | CalcBlock [CalcToken]
  deriving (Show, Eq)

type Ident = String

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

data Expr 
  = Float    Double
  | Int      Integer
  | BinOp    Op Expr Expr
  | Call     Ident [Expr]
  | Variable Ident
  deriving (Eq, Ord, Show)

data Stmt 
  = Function Ident [Stmt] Expr -- Name Parameters
  | VarDecl Ident (Maybe Expr)
  | InlineExpr Expr
  deriving(Eq, Ord, Show)

-- isOp :: Char -> Bool
-- isOp '+' = True
-- isOp '-' = True
-- isOp '*' = True
-- isOp '/' = True
-- isOp _   = False
-- 
-- opPrec :: Maybe Char -> Int
-- opPrec op = case op of
--   Just '+' -> 10
--   Just '-' -> 10 
--   Just '*' -> 20
--   Just '/' -> 20
--   Just _   -> -1
--   Nothing  -> -1