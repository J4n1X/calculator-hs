{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CalcInterpreter where

import CalcDefs
import CalcIntrinsics
import Text.Printf (printf)
import Control.Monad.State
import Control.Lens
import Data.Maybe
import Data.List (sort, sortBy, nub, foldl')


maxRecursive :: Int
maxRecursive = 20

-- This file contains the interpreting functions for the Calculator
data VariableScopeType =
    Local
  | Global
  deriving (Show, Eq)

type ScopeId = Int

data InterpVariable = InterpVariable {
  _variableName  :: Ident,
  _variableScope :: ScopeId,
  _variableValue :: Double
} deriving (Show, Eq)
makeLenses ''InterpVariable

sortVars :: InterpVariable -> InterpVariable -> Ordering
sortVars v1@(InterpVariable v1name v1scope _) v2@(InterpVariable v2name v2scope _) = compare (v1scope, v1name) (v2scope, v2name)

data InterpState = InterpState {
  _interpFunctions :: [Stmt],
  _interpVariables :: [InterpVariable],
  _interpCarry     :: Maybe Double,
  _interpScopeId   :: ScopeId
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

searchVariable :: Ident -> [InterpVariable] -> Maybe InterpVariable
searchVariable target vars = searchVariableContent target (sortBy (flip sortVars) vars)
  where
  byDepth (InterpVariable _ d1 _) (InterpVariable _ d2 _) = compare d1 d2
  searchVariableContent target (x:rem) =
    case x of
      InterpVariable name _ _ ->
        if name == target then
          Just x
        else
          searchVariableContent target rem
  searchVariableContent "" _ = Nothing
  searchVariableContent _ [] = Nothing

getVariable :: Ident ->ScopeId -> [InterpVariable] -> Maybe InterpVariable
getVariable target tgtdepth (x:vars) =
  case x of
    InterpVariable name depth _ ->
      if name == target && tgtdepth == depth then
        Just x
      else
        getVariable target depth vars
getVariable "" _ _ = Nothing
getVariable _ _ [] = Nothing

runMain :: InterpState -> Maybe Double
runMain state = view interpCarry $ interpStmt state $ fromMaybe (error "Main function wasn't found") (searchFunction "main" $ view interpFunctions state)

cleanVars :: InterpState -> InterpState
cleanVars st =
  let outState =  st & interpScopeId -~ 1 in
    outState & interpVariables %~ filter (\var -> outState ^. interpScopeId >= var ^. variableScope)

interpFunCall :: InterpState -> Expr -> InterpState
interpFunCall state c@(Call name args) =
  case searchFunction name functions of
    Just (Function _ params body) ->
      --if state ^. interpScopeId > maxRecursive then
      --  error $ "Reached max recursion, variable dump: " ++ show (state ^. interpVariables)
      --else
      --  cleanVars $ interpStmt (buildVars (state & interpScopeId +~ 1) params args) body
      cleanVars $ interpStmt (buildVars (state & interpScopeId +~ 1) params args) body
    _ -> interpExternCall state c
  where
    buildVars st params args = foldl' addVariable st (zipWith (\(VarDecl name _) b -> VarDecl name (Just b)) params args)
    addVariable st var@(VarDecl name (Just valExpr)) = 
      let newSt = interpExpr st valExpr in
        newSt & interpVariables %~ (++ [toInterpVar name newSt])
    addVariable _ _ = error "Unexpected variable state"
    variables = view interpVariables state
    functions = view interpFunctions state
    getVarName (VarDecl name _ ) = name
    getVarName _                 = error "Not a variable"
interpFunCall _ st = error $ "Expected Call but got " ++ show st

interpExternCall :: InterpState -> Expr -> InterpState
interpExternCall state (Call name args) =
  case length args of
        1 -> let newState = interpExpr state (head args) in
          newState
            & interpScopeId +~ 1
            & interpCarry ?~ intrinsic1 name (fromMaybe (error "Failed to parse argument for external function call") (newState ^. interpCarry))
            & interpScopeId -~ 1
        2 -> let leftState  = interpExpr state (head args) in
             let rightState = interpExpr state (args !! 1) in
              rightState
              & interpScopeId +~ 1
              & interpCarry ?~ intrinsic2 name
                (fromMaybe (error "Failed to parse first argument for external function call") (leftState ^. interpCarry))
                (fromMaybe (error "Failed to parse argument for external function call") (rightState ^. interpCarry))
              & interpScopeId -~ 1
        _ -> error "Invalid amount of args for intrinsic call"
interpExternCall _ st = error $ "Expected Call but got " ++ show st

toInterpVar :: Ident -> InterpState -> InterpVariable
toInterpVar name st = InterpVariable name (st ^. interpScopeId) (fromMaybe (error "Expression evaluated to nothing") $ st ^. interpCarry)

interpVarDecl :: InterpState -> Stmt -> InterpState
interpVarDecl state cur@(VarDecl name val) = 
  case getVariable name (state ^. interpScopeId) (state ^. interpVariables) of
    Just old@(InterpVariable name1 val1 scope1) -> replaceVarDecl state cur old
    Nothing -> 
      case searchVariable name (state ^. interpVariables) of -- TODO: Refactor
        Just old@(InterpVariable name1 val1 scope1) -> replaceVarDecl state cur old
        Nothing ->
          let valState = interpVarDeclVal state cur in
          valState & interpVariables %~ ( ++ [toInterpVar name valState])
  where
    interpVarDeclVal state st@(VarDecl name val) = case val of
        Just res ->
          let resVal = interpExpr state res in
            resVal
            --InterpVariable name (view interpScopeId resVal) (fromMaybe (error "Expression evaluated to nothing") $ view interpCarry resVal) 
        Nothing ->  error "Expected value"
    interpVarDeclVal _ _ = error "Expected Variable Declaration"

    replaceVarDecl :: InterpState -> Stmt -> InterpVariable -> InterpState
    replaceVarDecl vars cur@(VarDecl name val) old =
      let newState = interpVarDeclVal state cur in
        newState & interpVariables %~ map (\item -> if item == old then toInterpVar name newState else item)
        --zipWith (\ i v -> (if i == curVar then new else v)) (state ^. interpVariables) (state ^.interpVariable)
    replaceVarDecl _ _ _ = error "Expected Variable Declaration"
interpVarDecl _ _ = error "Expected Variable Declaration"

interpBlock :: InterpState -> Stmt -> InterpState
interpBlock state blk@Block {} = cleanVars $ interpBlockContent (state & interpScopeId +~ 1) blk
interpBlock _ st = error $ "Expected block, but got " ++ show st

interpBlockContent :: InterpState -> Stmt -> InterpState
interpBlockContent state (Block (x:rem)) = interpBlockContent (interpStmt state x) (Block rem)
interpBlockContent state (Block [])      = state
interpBlockContent _ st                  = error $ "Expected block, but got " ++ show st

interpIfCond :: InterpState -> Stmt -> InterpState
interpIfCond state (IfCond cond trueBody elseBody) =
  let condState = interpExpr state cond in
    if condState ^. interpCarry == Just 1.0 then
      interpStmt condState trueBody
    else
      case elseBody of
        Just body -> interpStmt condState body
        Nothing -> condState
interpIfCond _ st                                  = error $ "Exprected if statement, but got " ++ show st

interpFunDef :: InterpState -> Stmt -> InterpState
interpFunDef state fun@(Function name vars body) = state
                                              & interpFunctions %~ (++ [fun]) . filter (\(Function name1 _ _) -> name1 /= name) -- Remove old function if it exists
                                              & interpCarry .~ Nothing
interpFunDef _ st = error $ "Expected Function definition, got " ++ show st

interpExpr :: InterpState -> Expr -> InterpState
interpExpr state (BinOp op lhs rhs) =
  set interpCarry
    (addValues op (view interpCarry (interpExpr state lhs))
    (view interpCarry (interpExpr state rhs)))
    state
  where
    -- this is kind of a hack, but we'll use it for now
    boolResult :: Bool -> Double
    boolResult True  = 1.0
    boolResult False = 0.0
    addValues :: Op -> Maybe Double -> Maybe Double -> Maybe Double
    addValues op left right = case op of
      Plus         -> (+) <$> left <*> right
      Minus        -> (-) <$> left <*> right
      Times        -> (*) <$> left <*> right
      Divide       -> (/) <$> left <*> right
      Equal        -> boolResult <$> ((==) <$> left <*> right)
      NotEqual     -> boolResult <$> ((/=) <$> left <*> right)
      Greater      -> boolResult <$> ((>)  <$> left <*> right)
      GreaterEqual -> boolResult <$> ((>=) <$> left <*> right)
      Less         -> boolResult <$> ((<)  <$> left <*> right)
      LessEqual    -> boolResult <$> ((<=) <$> left <*> right)
interpExpr state (Float val) = set interpCarry (Just val) state

interpExpr state var@(Variable name) =
  set interpCarry
  (Just
  $ view variableValue
  $ fromMaybe (error $ "Failed to get variable " ++ show name ++ "\nVariable dump: " ++ show (state ^. interpVariables))
  $ searchVariable name (state ^. interpVariables))
  state
interpExpr state call@Call {}             = interpFunCall state call
interpExpr state _ = set interpCarry Nothing state

interpStmt :: InterpState -> Stmt -> InterpState
interpStmt state (StmtExpr ex)            = interpExpr state ex
interpStmt state fun@(Function name _ _)  = interpFunDef state fun
interpStmt state var@VarDecl {}           = interpVarDecl state var
interpStmt state block@Block {}           = interpBlock state block
interpStmt state ifc@IfCond {}            = interpIfCond state ifc
-- interpStmt _ st                           = error $ "Unexpected Statment Type: " ++ show st

interpTopLevel :: InterpState -> [Stmt] -> InterpState
interpTopLevel state = interpRecurse state
  where
    interpRecurse state (x:rem) = interpRecurse (interpStmt state x) rem
    interpRecurse state []      = state