{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module CalcInterpreter where

import CalcDefs
import CalcParser
import Text.Printf (printf)

-- This file contains the interpreting functions for the Calculator

interpExpr :: Expr -> Maybe Double
interpExpr (BinOp op lhs rhs) = case op of
  Plus   -> (+) <$> interpExpr lhs <*> interpExpr rhs
  Minus  -> (-) <$> interpExpr lhs <*> interpExpr rhs
  Times  -> (*) <$> interpExpr lhs <*> interpExpr rhs
  Divide -> (/) <$> interpExpr lhs <*> interpExpr rhs
interpExpr (Float val) = Just val
interpExpr _ = Nothing