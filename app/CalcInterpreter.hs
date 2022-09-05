{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CalcInterpreter where

import CalcDefs
import CalcParser
import Text.Printf (printf)
import Control.Applicative
import Control.Monad.State

-- This file contains the interpreting functions for the Calculator


data InterpState = InterpState {
  _interpFunctions :: [Stmt],
  _interpVariables :: [Stmt]
} deriving (Show, Eq)

newtype Interpreter a = Interpreter {
  runInterpreter :: State InterpState a
} deriving (Functor, Applicative, Monad, MonadState InterpState)


interpExpr :: Expr -> Maybe Double
interpExpr (BinOp op lhs rhs) = case op of
  Plus   -> (+) <$> interpExpr lhs <*> interpExpr rhs
  Minus  -> (-) <$> interpExpr lhs <*> interpExpr rhs
  Times  -> (*) <$> interpExpr lhs <*> interpExpr rhs
  Divide -> (/) <$> interpExpr lhs <*> interpExpr rhs
interpExpr (Float val) = Just val
interpExpr _ = Nothing