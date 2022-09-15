module CalcIntrinsics where

import CalcDefs
import Data.Char

-- data IntrinsicFunction = 
--       Arg1 String (Double -> Double)
--     | Arg2 String (Double -> Double -> Double)

intrinsic1 :: Floating a => Ident -> (a -> a)
intrinsic1 = f
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

intrinsic2 :: Ident -> (Double -> Double -> Double)
intrinsic2 = f
  where
    f name
      | name == "(**)" = (**)
      | name == "logBase" = logBase
      | otherwise = error "Unknown double-argument intrinsic"