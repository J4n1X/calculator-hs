module CalcIntrinsics where

import CalcDefs
import Data.Char

-- data IntrinsicFunction = 
--       Arg1 String (Double -> Double)
--     | Arg2 String (Double -> Double -> Double)

intrinsic1 :: Ident -> (CalcValue -> CalcValue)
intrinsic1 = asCalcValues . f
  where
    f name
      | name == "exp"   = exp
      | name == "sqrt"  = sqrt
      | name == "log"   = log
      | name == "sin"   = sin
      | name == "tan"   = tan
      | name == "cos"   = cos
      | name == "asin"  = asin
      | name == "atan"  = atan
      | name == "acos"  = acos
      | name == "sinh"  = sinh
      | name == "tanh"  = tanh
      | name == "cosh"  = cosh
      | name == "asinh" = asinh
      | name == "atanh" = atanh
      | name == "acosh" = acosh
      | otherwise = error "Unknown single-argument intrinsic"
    asCalcValues :: (Double -> Double) -> CalcValue -> CalcValue
    asCalcValues f (FloatValue val) =  FloatValue $ f val
    asCalcValues _ val              = error $ "Expected Float value, but got " ++ show val

intrinsic2 :: Ident -> (CalcValue -> CalcValue -> CalcValue)
intrinsic2 = asCalcValues . f
  where
    f name
      | name == "pow" = (**)
      | name == "logBase" = logBase
      | otherwise = error "Unknown double-argument intrinsic"
    asCalcValues :: (Double -> Double -> Double) -> CalcValue -> CalcValue -> CalcValue
    asCalcValues f (FloatValue val1) (FloatValue val2) =  FloatValue $ f val1 val2
    asCalcValues _ _ val              = error $ "Expected Float value, but got " ++ show val