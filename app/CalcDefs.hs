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
  | Equal
  | NotEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  deriving (Eq, Ord, Show)

data Expr 
  = Float    Double
  | Int      Integer
  | Call     Ident [Expr]
  | BinOp    Op Expr Expr
  | Variable Ident
  deriving (Eq, Ord, Show)

data Stmt 
  = IfCond Expr Stmt (Maybe Stmt)  -- Condition True-Block False-Block
  | Function Ident [Stmt] Stmt -- Name Parameters Block
  | VarDecl Ident (Maybe Expr) -- Name Value(Optional)
  | StmtExpr Expr              -- Expression with discarded value
  | Block [Stmt]               -- List of Statements
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