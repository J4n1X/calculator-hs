{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module CalcInterpreter
  ( InterpData (..),
    InterpState (..),
    interpNumber,
    interpOperation,
    interpExpr,
    nextExpr
  )
where

import CalcDefs
import CalcParser
import Control.Applicative
import Data.Bifunctor (Bifunctor)
import GHC.Generics
import Text.Printf (printf)
import Control.Lens

-- This file contains the interpreting functions for the Calculator

data InterpData = InterpData
  { interpPos :: Int,
    interpToks :: [CalcToken]
  }
  deriving (Show)
makeLenses ''InterpData

data InterpState a b = InterpState
  { _stateCarry :: a,
    _stateTokens :: [b]
  }
  deriving (Show, Generic)
makeLenses ''InterpState

newtype InterpError = InterpError String deriving (Show)

carryOp :: Fractional a => Char -> (a -> a -> a)
carryOp '+' = (+)
carryOp '-' = (-)
carryOp '*' = (*)
carryOp '/' = (/)
carryOp _ = error "Invalid Operation"

nextStateToken :: InterpState a b -> Maybe (InterpState a b, b)
nextStateToken (InterpState carry state@(tok : rem)) = Just (InterpState carry rem, tok)
nextStateToken (InterpState _ []) = Nothing

--setStateCarry :: InterpState a b -> a -> InterpState a b
--setStateCarry state val = InterpState val (stateTokens state)

splitState :: InterpState Double b -> InterpState Double b
splitState (InterpState _ tokens) = InterpState 0.0 tokens

mapStateCarry :: Fractional a => Char -- ^ Operator
  -> InterpState a b -- ^ LHS
  -> InterpState a b -- ^ RHS
  -> InterpState a b
mapStateCarry op (InterpState carryA _) stateB@(InterpState carryB _) =
  set stateCarry (carryOp op carryA carryB) stateB

interpNumber :: InterpState Double CalcToken -> Maybe (InterpState Double CalcToken)
interpNumber state = case nextStateToken state of
  Just (rem, CalcNumber tok) -> Just $ set stateCarry tok rem
  Just (_, tok) -> Nothing
  Nothing -> Nothing

interpBlock :: InterpState Double CalcToken -> Maybe (InterpState Double CalcToken)
interpBlock state =
  case nextStateToken state of
    Just (rem, CalcBlock tok) -> --Just $ setStateCarry rem $ stateCarry $ interpOperation (interpExpr $ InterpState 0 tok) 0
      let nextExpr = interpOperation (interpExpr $ InterpState 0 tok) 0 in
        Just $ set stateTokens (view stateTokens rem) nextExpr

    Just (_, tok) -> Nothing
    Nothing -> Nothing

interpExpr :: InterpState Double CalcToken -> InterpState Double CalcToken
interpExpr state =
  case interpBlock state <|> interpNumber state of
    Just ret -> ret
    Nothing -> error ("Block parsing failed! State dump: \n" ++ show state)


-- Just interprete together
interpBinOperation :: InterpState Double CalcToken -- ^ LHS
  -> InterpState Double CalcToken -- ^ RHS
  -> Char -- ^ Operator
  -> InterpState Double CalcToken
interpBinOperation lhs rhs op = mapStateCarry op lhs rhs

nextExpr :: Int -- ^ Previous Operator Precedence
  -> InterpState Double CalcToken -- ^ Current State
  -> InterpState Double CalcToken
nextExpr prevOpPrec state = 
  -- interp next carry, then get operator
  case nextStateToken state of 
    Just(opState, CalcOperator curOp) -> 
      if opPrec (Just curOp) > prevOpPrec then
        mapStateCarry curOp state $ nextExpr (opPrec $ Just curOp) (interpExpr opState)
      else
        state
    Just(_, _) -> state
    Nothing -> state

interpOperation :: InterpState Double CalcToken -> Int -> InterpState Double CalcToken
interpOperation state@(InterpState _ toks) prevPrec = 
  case nextStateToken state of
    Just(_, CalcOperator _) -> interpOperation (nextExpr prevPrec state) 0
    Just(_, _)              -> interpOperation (nextExpr prevPrec $ interpExpr state) 0
    Nothing -> state