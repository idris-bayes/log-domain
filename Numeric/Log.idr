module Numeric.Log

||| Numeric utility functions
export
isInf : (FromDouble a, Eq a, Fractional a) => a -> Bool
isInf = (1.0/0.0 ==)

export
negInf : (Neg a, Fractional a) => a
negInf = -(1/0)

%foreign "C:c_log1p,libm"
c_log1p : Double -> Double

log1p = c_log1p

log1pexp : Double -> Double
log1pexp a = log1p (exp a)

log1mexp : Double -> Double
log1mexp a = log1p (negate (exp a))

||| Log Domain
public export
record Log (a : Type) where
  constructor Exp
  ln : a

public export
Functor Log where
  map f (Exp a) = Exp (f a)

public export
Num (Log Double) where
  fromInteger = Exp . log . fromInteger
  Exp a * Exp b = Exp (a + b)
  Exp a + Exp b =
    if a == b && isInf a && isInf b then Exp a else
    if a >= b then Exp (a + log1pexp (b - a)) else
    Exp (b + log1pexp (a - b))

public export
Abs (Log Double) where
  abs = id

public export
Neg (Log Double) where
  negate (Exp a) = if isInf a && a < 0 then Exp negInf else Exp (0/0)
  Exp a - Exp b = if isInf a && isInf b && a < 0 && b < 0 then Exp negInf else Exp (a + log1mexp (b - a))

public export
Fractional (Log Double) where
  Exp a / Exp b = Exp (a - b)