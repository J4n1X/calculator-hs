{-# LANGUAGE TemplateHaskell #-}
module CalcDefs where

import Data.Int
import Data.Word
import Control.Lens
import Data.List (sortBy)
import Debug.Trace
-- This file contains important structures and functions for the base of the calculator

{- 
  We will support the following primitives:
    - 64-Bit signed integer
    - 8-Bit unsigned integer (Byte)  | Needs cast
    - Floating point number (Double)
    - Nothing (void)
    - Static size list of any type
-}
type ScopeId = Int



data CalcType
  = CalcInteger
  | CalcByte
  | CalcFloat
  | CalcVoid
  | CalcArray !CalcType !Integer
  deriving (Show, Eq, Ord)

data CalcValue 
  = IntegerValue !Int64
  | ByteValue    !Word8
  | FloatValue   !Double
  | ArrayValue   ![CalcValue]
  deriving (Show, Eq, Ord)

getValueType :: CalcValue -> CalcType
getValueType IntegerValue {} = CalcInteger
getValueType ByteValue {} = CalcByte
getValueType FloatValue {} = CalcFloat
getValueType (ArrayValue vals) = if not (null vals) then getValueType (head vals) else error "Array is uninitialized"

valueIsType :: CalcValue -> CalcType -> Bool
valueIsType IntegerValue {} CalcInteger           = True
valueIsType ByteValue {} CalcByte                 = True
valueIsType FloatValue {} CalcFloat               = True
valueIsType (ArrayValue vals) (CalcArray typ len) = length vals == fromIntegral len && if not (null vals) then valueIsType (head vals) typ else error "Array is uninitialized"
valueIsType _ _                                   = False

instance Num CalcValue where
  (+) (IntegerValue v1) (IntegerValue v2) = IntegerValue $ v1 + v2
  (+) (ByteValue v1) (ByteValue v2)       = ByteValue    $ v1 + v2
  (+) (FloatValue v1) (FloatValue v2)     = FloatValue   $ v1 + v2
  (+) v1 v2                               = error        $ "Tried to apply addition/subtraction operation to value of type " ++ show v1 ++ " with other value " ++ show v2
  (*) (IntegerValue v1) (IntegerValue v2) = IntegerValue $ v1 * v2
  (*) (ByteValue v1) (ByteValue v2)       = ByteValue    $ v1 * v2
  (*) (FloatValue v1) (FloatValue v2)     = FloatValue   $ v1 * v2
  (*) v1 v2                               = error        $ "Tried to apply multiplication operation to value of type " ++ show v1 ++ " with other value " ++ show v2
  abs (IntegerValue v1) = IntegerValue $ abs v1
  abs (ByteValue v1)    = ByteValue    $ abs v1
  abs (FloatValue v1)   = FloatValue   $ abs v1
  abs v1                = error        $ "Attempted to apply abs operation to value of type " ++ show v1
  signum (IntegerValue v1) = IntegerValue $ signum v1
  signum (ByteValue v1)    = ByteValue    $ signum v1
  signum (FloatValue v1)   = FloatValue   $ signum v1
  signum v1                = error        $ "Attempted to apply signum operation to value of type " ++ show v1
  fromInteger = IntegerValue . fromInteger
  negate (IntegerValue v1) = IntegerValue $ negate v1
  negate (ByteValue v1)    = ByteValue    $ negate v1
  negate (FloatValue v1)   = FloatValue   $ negate v1
  negate v1                = error        $ "Attempted to apply negate operation to value of type " ++ show v1

instance Fractional CalcValue where
  fromRational = FloatValue . fromRational
  (/) (IntegerValue v1) (IntegerValue v2) = IntegerValue $ v1 `div` v2
  (/) (ByteValue v1) (ByteValue v2)       = ByteValue    $ v1 `div` v2
  (/) (FloatValue v1) (FloatValue v2)     = FloatValue   $ v1 / v2
  (/) v1 v2                               = error        $ "Tried to apply multiplication operation to value of type " ++ show v1 ++ " with other value " ++ show v2

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
 = IfCond   Expr   Stmt   (Maybe Stmt)           -- Condition True-Block False-Block
 | Function Ident (Maybe CalcType) [Stmt] Stmt   -- Name Parameters Block
 | VarDecl  Ident (Maybe CalcType)  (Maybe Expr) -- Name Value(Optional)
 | StmtExpr Expr                                 -- Expression with discarded value
 | Block    [Stmt]                               -- List of Statements
  deriving(Eq, Ord, Show)

data InterpVariable = InterpVariable {
  _variableName  :: Ident,
  _variableScope :: ScopeId,
  _variableType  :: CalcType,
  _variableValue :: CalcValue
} deriving (Show, Eq)
makeLenses ''InterpVariable

data InterpState = InterpState {
  _interpFunctions :: [Stmt],
  _interpVariables :: [InterpVariable],
  _interpCarry     :: Maybe CalcValue,
  _interpScopeId   :: ScopeId
} deriving (Show, Eq)
makeLenses ''InterpState

sortVars :: InterpVariable -> InterpVariable -> Ordering
sortVars v1@(InterpVariable v1name v1scope v1type _) v2@(InterpVariable v2name v2scope v2type _) = compare (v1scope, v1name) (v2scope, v2name)

searchFunction :: String -> [Stmt] -> Maybe Stmt
searchFunction target (x:funs) =
  case x of
    Function name _ _ _ ->
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
  byDepth (InterpVariable _ _ d1 _) (InterpVariable _ _ d2 _) = compare d1 d2
  searchVariableContent target (x:rem) =
    case x of
      InterpVariable name _ _ _ ->
        if name == target then
          --trace ("Returning Variable " ++ show x) Just x
          Just x
        else
          searchVariableContent target rem
  searchVariableContent "" _ = Nothing
  searchVariableContent _ [] = Nothing

getVariable :: Ident ->ScopeId -> [InterpVariable] -> Maybe InterpVariable
getVariable target tgtdepth (x:vars) =
  case x of
    InterpVariable name depth _ _ ->
      if name == target && tgtdepth == depth then
        Just x
      else
        getVariable target depth vars
getVariable "" _ _ = Nothing
getVariable _ _ [] = Nothing
