{-# LANGUAGE FlexibleInstances #-}

module CalcInterpreter where

import CalcDefs
import CalcIntrinsics
import Text.Printf (printf)
import Data.Maybe ( fromMaybe, isNothing, fromJust )
import Control.Lens
import Data.List (foldl')
import Debug.Trace
maxRecursive :: Int
maxRecursive = 0


runMain :: InterpState -> Maybe CalcValue
runMain state = view interpCarry $ interpStmt state $ fromMaybe (error "Main function wasn't found") (searchFunction "main" $ view interpFunctions state)

cleanVars :: InterpState -> InterpState
cleanVars st =
  let outState =  st & interpScopeId -~ 1 in
    outState & interpVariables %~ filter (\var -> outState ^. interpScopeId >= var ^. variableScope)

-- TODO: SUPPORT FOR INITIALIZER LATER
-- TODO: Check arg count match
interpFunCall :: InterpState -> Expr -> InterpState
interpFunCall state c@(Call name args) =
  case searchFunction name functions of
    Just (Function _ expectedTyp params body) ->
      if state ^. interpScopeId > maxRecursive && maxRecursive > 0 then
        error $ "Reached max recursion, variable dump: " ++ show (state ^. interpVariables)
      else
        let retState = cleanVars $ interpStmt (buildVars (state & interpScopeId +~ 1) params args) body in
        let retTyp = getValueType <$> (retState ^. interpCarry) in
          if retTyp == expectedTyp then
            retState
          else
            error $ "Type mismatch in function return; Expected " ++ show (fromJust expectedTyp) ++ " but got type " ++ show (fromJust retTyp)
      --  cleanVars $ interpStmt (buildVars (state & interpScopeId +~ 1) params args) body
      
    _ -> interpExternCall state c
  where
    buildVars st params args = foldl' addVariable st (zipWith (\(VarDecl name typ _) b -> VarDecl name typ (Just b)) params args)
    addVariable st var@(VarDecl name typ (Just valExpr)) = 
      let newSt = interpExpr st valExpr in
        newSt & interpVariables %~ (++ [toInterpVar name (fromMaybe (error "Expected Type in param declaration") typ) newSt])
    addVariable _ _ = error "Unexpected variable state"
    variables = view interpVariables state
    functions = view interpFunctions state
    getVarName (VarDecl name _ _ ) = name
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

toInterpVar :: Ident -> CalcType -> InterpState -> InterpVariable
toInterpVar name typ st = InterpVariable name (st ^. interpScopeId) typ (fromMaybe (error "Expression evaluated to nothing") $ st ^. interpCarry)

interpVarDecl :: InterpState -> Stmt -> InterpState
interpVarDecl state cur@(VarDecl name typ val) = 
  case getVariable name (state ^. interpScopeId) (state ^. interpVariables) of
    Just old@(InterpVariable name1 val1 typ1 scope1) -> 
      if typ == Just typ1 || isNothing typ then
        replaceVarDecl state cur old
      else
        error $ "Type mismatch while trying to update variable " ++ show old
    Nothing -> 
      case searchVariable name (state ^. interpVariables) of -- TODO: Refactor
        Just old@(InterpVariable name1 val1 type1 scope1) -> replaceVarDecl state cur old
        Nothing ->
          let valState = interpVarDeclVal state cur in
          valState & interpVariables %~ ( ++ [toInterpVar name (fromMaybe (error "Expected type at variable declaration") typ)  valState])
  where
    interpVarDeclVal state st@(VarDecl name typ val) = case val of
        Just res ->
          let resVal = interpExpr state res in
            if (getValueType <$> resVal ^. interpCarry)  == typ then
              resVal
            else
              error $ "Type mismatch: Tried to assign variable " ++ show st ++ " a value of type " ++ show (getValueType <$> resVal ^. interpCarry)
            --InterpVariable name (view interpScopeId resVal) (fromMaybe (error "Expression evaluated to nothing") $ view interpCarry resVal) 
        Nothing ->  error $ "Expected value for " ++ show st
    interpVarDeclVal _ _ = error "Expected Variable Declaration"

    replaceVarDecl :: InterpState -> Stmt -> InterpVariable -> InterpState
    replaceVarDecl vars cur@(VarDecl name typ val) old =
      let newState = interpVarDeclVal state cur in
        newState & interpVariables %~ map (\item -> if item == old then toInterpVar name (fromMaybe (error "Expected type in variable definition") typ) newState else item)
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
    if condState ^. interpCarry == Just (IntegerValue 1) then
      interpStmt condState trueBody
    else
      case elseBody of
        Just body -> interpStmt condState body
        Nothing -> condState
interpIfCond _ st = error $ "Exprected if statement, but got " ++ show st

interpFunDef :: InterpState -> Stmt -> InterpState
interpFunDef state fun@(Function name typ vars body) = state
                                              & interpFunctions %~ (++ [fun]) . filter (\(Function name1 _ _ _) -> name1 /= name) -- Remove old function if it exists
                                              & interpCarry .~ Nothing
interpFunDef _ st = error $ "Expected Function definition, got " ++ show st

addValues :: Op -> Maybe CalcValue -> Maybe CalcValue -> Maybe CalcValue
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
  where
    -- this is kind of a hack, but we'll use it for now
    boolResult :: Bool -> CalcValue
    boolResult True  = IntegerValue 1
    boolResult False = IntegerValue 0

interpExpr :: InterpState -> Expr -> InterpState
interpExpr state (BinOp op lhs rhs) =
  set interpCarry
    (addValues op (view interpCarry (interpExpr state lhs))
    (view interpCarry (interpExpr state rhs)))
    state 
interpExpr state (Value val) = set interpCarry (Just val) state
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
interpStmt state (StmtExpr ex)   = interpExpr state ex
interpStmt state fun@Function {} = interpFunDef state fun
interpStmt state var@VarDecl {}  = interpVarDecl state var
interpStmt state block@Block {}  = interpBlock state block
interpStmt state ifc@IfCond {}   = interpIfCond state ifc
-- interpStmt _ st                           = error $ "Unexpected Statment Type: " ++ show st

interpTopLevel :: InterpState -> [Stmt] -> InterpState
interpTopLevel state = interpRecurse state
  where
    interpRecurse state (x:rem) = interpRecurse (interpStmt state x) rem
    interpRecurse state []      = state