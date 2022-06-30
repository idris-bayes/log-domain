module Numeric.Log

||| Numeric utility functions
export
isInf : (FromDouble a, Eq a, Fractional a) => a -> Bool
isInf = (1.0/0.0 ==)

export
negInf : (Neg a, Fractional a) => a
negInf = -(1/0)

%foreign "C:c_log1p,log-domain"
c_log1p : Double -> Double

%foreign "C:c_expm1,log-domain"
c_expm1 : Double -> Double

log1pexp : Double -> Double
log1pexp a = c_log1p (exp a)

log1mexp : Double -> Double
log1mexp a = c_log1p (negate (exp a))

||| Log Domain
public export
record Log (a : Type) where
  constructor Exp
  ln : a


public export
Show (Log Double) where
  show (Exp a) = show (exp a)

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

public export
Eq a => Eq (Log a) where
  Exp a == Exp b = a == b 

public export
Ord a => Ord (Log a) where
  Exp a < Exp b = a < b

public export
interface ToLogDomain a where
  toLogDomain : a -> Log Double

public export
ToLogDomain Double where
  toLogDomain = Exp . log

public export
ToLogDomain Nat where
  toLogDomain = Exp . log . cast

public export
ToLogDomain Int where
  toLogDomain = Exp . log . cast

public export
ToLogDomain Integer where
  toLogDomain = Exp . log . cast

public export
||| Intentionally compares (Exp x) with negInf via "exp x > 0", rather than via "x > negInf". 
isPositive : Log Double -> Bool
isPositive z = (exp . ln) z > 0

||| Efficiently and accurately compute the sum of a set of log-domain numbers
data Acc a = MkAcc {-# UNPACK #-} Int64 a | None

export
sum : (Foldable f) => f (Log Double) -> Log Double
sum xs = Exp $ case foldl step1 None xs of
  None => negInf
  MkAcc nm1 a => 
    if isInf a then a
    else a + c_log1p (foldl (step2 a) 0 xs + cast nm1)
  where
    step1 : Acc Double -> Log Double -> Acc Double
    step1 None      (Exp x) = MkAcc 0 x
    step1 (MkAcc n y) (Exp x) = MkAcc (n + 1) (max x y)
    step2 : Double -> Double -> Log Double -> Double
    step2 a r (Exp x) = r + c_expm1 (x - a)