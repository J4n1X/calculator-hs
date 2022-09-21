module CalcDefs where

import Data.Int
import Data.Word

-- This file contains important structures and functions for the base of the calculator

{- 
  We will support the following primitives:
    - 64-Bit signed integer
    - 8-Bit unsigned integer (Byte)
    - Floating point number (Double)
    - Static size list of any type
-}
data CalcType
  = CalcInteger
  | CalcByte
  | CalcFloat
  | CalcArray CalcType Integer
  deriving (Show, Eq, Ord)

data CalcValue 
  = IntegerValue Int64
  | ByteValue    Word8
  | FloatValue   Double
  | ArrayValue   CalcValue
  deriving (Show, Eq, Ord)

data CalcToken
  = CalcValue    CalcValue   
  | CalcOperator Char
  | CalcBlock    [CalcToken]
  deriving (Show, Eq, Ord)

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
  = Value    CalcValue
  | Call     Ident [Expr]
  | BinOp    Op Expr Expr
  | Variable Ident
  deriving (Eq, Ord, Show)

data Stmt 
 = IfCond   Expr   Stmt   (Maybe Stmt)   -- Condition True-Block False-Block
 | Function Ident  [Stmt] Stmt           -- Name Parameters Block            
 | VarDecl  Ident  (Maybe Expr)          -- Name Value(Optional)             
 | StmtExpr Expr                         -- Expression with discarded value             
 | Block    [Stmt]                       -- List of Statements              
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