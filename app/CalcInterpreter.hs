{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CalcInterpreter where

import CalcDefs
import CalcParser
import Text.Printf (printf)
import Control.Applicative
import Control.Monad.State
import Control.Lens
import Data.Maybe

-- This file contains the interpreting functions for the Calculator
data InterpVariable = InterpVariable {
  _variableName  :: Ident,
  _variableValue :: Double
} deriving (Show, Eq)
makeLenses ''InterpVariable

data InterpState = InterpState {
  _interpFunctions :: [Stmt],
  _interpVariables :: [InterpVariable],
  _interpCarry     :: Maybe Double
} deriving (Show, Eq)
makeLenses ''InterpState

newtype Interpreter a = Interpreter {
  runInterpreter :: State InterpState a
} deriving (Functor, Applicative, Monad, MonadState InterpState)

searchFunction :: String -> [Stmt] -> Maybe Stmt
searchFunction target (x:funs) =
  case x of
    Function name _ _ ->
      if name == target then
        Just x
      else
        searchFunction target funs
    _ -> error "Unexpected Non-Function in list"
searchFunction "" _ = Nothing
searchFunction _ [] = Nothing

searchVariable :: String -> [InterpVariable] -> Maybe InterpVariable
searchVariable target (x:vars) =
  case x of
    InterpVariable name _ ->
      if name == target then
        Just x
      else
        searchVariable target vars
searchVariable "" _ = Nothing
searchVariable _ [] = Nothing

removeVariable :: Ident -> [InterpVariable] -> [InterpVariable]
removeVariable tgt = filter (not <$> compareName tgt)
  where
    compareName name var = name == view variableName var

runMain :: InterpState -> Maybe Double
runMain state = view interpCarry $ interpStmt state $ fromMaybe (error "Main function wasn't found") (searchFunction "main" $ view interpFunctions state)

assignVars :: InterpState -> [Stmt] -> [Expr] -> [InterpVariable]
assignVars state vars vals = zipWith InterpVariable (map varDefName vars) (map (fromJust . view interpCarry . interpExpr state) vals)
  where
    varDefName (VarDecl name _) = name
    varDefName _ = error "Not a variable"
    fromJust = fromMaybe (error "Maybe.fromJust: Nothing")

interpFunCall :: InterpState -> Expr -> InterpState
interpFunCall state (Call name args) =
  case searchFunction name functions of
    Just (Function _ params body) ->
      cleanVars params (interpExpr (appendVars $ assignVars state params args) body)
    _ -> error $ "Function was not found: " ++ show name
  where
    getVarName (VarDecl name _ ) = name
    getVarName _                 = error "Not a variable"

    cleanVars (x:params) state = cleanVars params $ set interpVariables (removeVariable (getVarName x) (view interpVariables state)) state
    cleanVars [] state = state

    isLocalVar :: [Stmt] -> InterpVariable -> Bool
    isLocalVar (x:params) var = (view variableName var == getVarName x) || isLocalVar params var
    isLocalVar [] _           = False

    appendVars new = set interpVariables (variables ++ new) state

    variables = view interpVariables state
    functions = view interpFunctions state
interpFunCall _ _ = error "Expected Call"

interpVarDecl :: InterpState -> Stmt -> InterpState
interpVarDecl state cur@(VarDecl name val) = case searchVariable name (view interpVariables state) of
  Just new@(InterpVariable name1 val1) -> set interpVariables (replaceVarDecl (view interpVariables state) cur new) state
  Nothing -> over interpVariables ( ++ [toInterpVariable state cur]) state
  where
    toInterpVariable state st@(VarDecl name val) = InterpVariable name $ case val of
      Just res -> fromMaybe (error "Expression evaluated to nothing") $ view interpCarry (interpExpr state res)
      Nothing ->  error "Expected value"
    toInterpVariable _ _ = error "Expected Variable Declaration"
    replaceVarDecl :: [InterpVariable] -> Stmt -> InterpVariable -> [InterpVariable]
    replaceVarDecl vars cur@(VarDecl name val) new =
      let curVar = toInterpVariable state cur in
        zipWith (\ i v -> (if i == curVar then new else v)) vars vars
    replaceVarDecl _ _ _ = error "Expected Variable Declaration"
interpVarDecl _ _ = error "Expected Variable Declaration"

interpExpr :: InterpState -> Expr -> InterpState
interpExpr state (BinOp op lhs rhs) =
  set interpCarry
    (addValues op (view interpCarry (interpExpr state lhs))
    (view interpCarry (interpExpr state rhs)))
    state
  where
    addValues :: Op -> Maybe Double -> Maybe Double -> Maybe Double
    addValues op left right = case op of
      Plus   -> (+) <$> left <*> right
      Minus  -> (-) <$> left <*> right
      Times  -> (*) <$> left <*> right
      Divide -> (/) <$> left <*> right
interpExpr state (Float val)         = set interpCarry (Just val) state
interpExpr state var@(Variable name) =
  set interpCarry
  (Just
  $ view variableValue
  $ fromMaybe (error "Failed to get variable")
  $ searchVariable name (view interpVariables state))
  state
interpExpr state call@Call {}        = interpFunCall state call
interpExpr state _ = set interpCarry Nothing state

interpStmt :: InterpState -> Stmt -> InterpState
interpStmt state (InlineExpr ex) = interpExpr state ex
interpStmt state fun@Function {} = over interpFunctions (++ [fun]) state
interpStmt state var@VarDecl {}  = interpVarDecl state var
interpStmt _ st = error $ "Unexpected Statment Type: " ++ show st

interpTopLevel :: [Stmt] -> InterpState
interpTopLevel = interpRecurse (InterpState [] [] $ Just 0)
  where
    interpRecurse state (x:rem) = interpRecurse (interpStmt state x) rem
    interpRecurse state []      = state