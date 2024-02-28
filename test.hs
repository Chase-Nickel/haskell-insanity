{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- for stdio only
import Prelude qualified as Pr

nil = Pr.undefined

data Bool = True | False

prBoolToBool :: Pr.Bool -> Bool
prBoolToBool Pr.True = True
prBoolToBool Pr.False = False

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

not :: Bool -> Bool
not True = False
not False = True

and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

nand :: Bool -> Bool -> Bool
nand b1 b2 = not (and b1 b2)

or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or False False = False

nor :: Bool -> Bool -> Bool
nor b1 b2 = not (or b1 b2)

xor :: Bool -> Bool -> Bool
xor True True = False
xor False True = True
xor True False = True
xor False False = False

xnor :: Bool -> Bool -> Bool
xnor b1 b2 = not (xor b1 b2)

data Integer = P Integer | Z | S Integer

prIntToInteger :: Pr.Int -> Integer
prIntToInteger 0 = Z
prIntToInteger n =
  if'
    (prBoolToBool (n Pr.< 0))
    (P (prIntToInteger (n Pr.+ 1)))
    (S (prIntToInteger (n Pr.- 1)))

i = prIntToInteger

integerNormalize :: Integer -> Integer
integerNormalize Z = Z
integerNormalize (S Z) = S Z
integerNormalize (P Z) = P Z
integerNormalize (P (S n)) = integerNormalize n
integerNormalize (S (P n)) = integerNormalize n
integerNormalize (P (P n)) = P (integerNormalize (P n))
integerNormalize (S (S n)) = S (integerNormalize (S n))

integerEQ :: Integer -> Integer -> Bool
integerEQ Z Z = True
integerEQ Z _ = False
integerEQ _ Z = False
integerEQ (S a) (S b) = integerEQ a b
integerEQ (P a) (P b) = integerEQ a b
integerEQ (S a) b = integerEQ a (P b)
integerEQ a (S b) = integerEQ (P a) b

integerLT :: Integer -> Integer -> Bool
integerLT Z Z = False
integerLT (S a) Z = False
integerLT (P a) Z = True
integerLT Z (S b) = True
integerLT Z (P b) = False
integerLT (S a) (S b) = integerLT a b
integerLT (P a) (P b) = integerLT a b
integerLT (S a) b = integerLT a (P b)
integerLT a (S b) = integerLT (P a) b

integerLTE :: Integer -> Integer -> Bool
integerLTE a b = or (integerEQ a b) (integerLT a b)

integerGT :: Integer -> Integer -> Bool
integerGT Z Z = False
integerGT (S a) Z = True
integerGT (P a) Z = False
integerGT Z (S b) = False
integerGT Z (P b) = True
integerGT (S a) (S b) = integerGT a b
integerGT (P a) (P b) = integerGT a b
integerGT (S a) b = integerGT a (P b)
integerGT a (S b) = integerGT (P a) b

integerGTE :: Integer -> Integer -> Bool
integerGTE a b = or (integerEQ a b) (integerGT a b)

integerAdd :: Integer -> Integer -> Integer
integerAdd Z Z = Z
integerAdd a Z = a
integerAdd Z b = b
integerAdd (S a) (S b) = integerAdd (S (S a)) b
integerAdd (P a) (P b) = integerAdd (P (P a)) b
integerAdd (S a) (P b) = integerAdd a b
integerAdd (P a) (S b) = integerAdd a b

integerAddN :: Integer -> Integer -> Integer
integerAddN a b = integerNormalize (integerAdd a b)

integerNeg :: Integer -> Integer
integerNeg Z = Z
integerNeg (S n) = P (integerNeg n)
integerNeg (P n) = S (integerNeg n)

integerSub :: Integer -> Integer -> Integer
integerSub Z Z = Z
integerSub a Z = a
integerSub Z b = integerNeg b
integerSub (S a) (S b) = integerSub a b
integerSub (P a) (P b) = integerSub a b
integerSub (S a) (P b) = integerSub (S (S a)) b
integerSub (P a) (S b) = integerSub (P (P a)) b

integerSubN :: Integer -> Integer -> Integer
integerSubN a b = integerNormalize (integerSub a b)

integerMul :: Integer -> Integer -> Integer
integerMul Z _ = Z
integerMul _ Z = Z
integerMul (S Z) b = b
integerMul a (S Z) = a
integerMul (S a) (S b) = integerAdd (S a) (integerMul (S a) b)
integerMul (P a) (P b) =
  integerAdd
    (S (integerNeg a))
    ( integerMul
        (S (integerNeg a))
        (integerNeg b)
    )
integerMul (S a) (P b) =
  integerNeg
    ( integerAdd
        (S a)
        (integerMul (S a) (integerNeg b))
    )
integerMul (P a) (S b) =
  integerNeg
    ( integerAdd
        (S (integerNeg a))
        (integerMul (S (integerNeg a)) b)
    )

integerMulN :: Integer -> Integer -> Integer
integerMulN a b = integerNormalize (integerMul a b)

integerFloorDiv :: Integer -> Integer -> Integer
integerFloorDiv a b = nil

integerFloorDivN :: Integer -> Integer -> Integer
integerFloorDivN a b = integerNormalize (integerFloorDiv a b)

(+) = integerAddN

(-) = integerSubN

(*) = integerMulN

(/) = integerFloorDivN

(==) = integerEQ

(<) = integerLT

(>) = integerGT

(<=) = integerLTE

(>=) = integerGTE

-- stdio

integerToPrInt :: Integer -> Pr.Int
integerToPrInt Z = 0
integerToPrInt (S n) = 1 Pr.+ integerToPrInt n
integerToPrInt (P n) = (-1) Pr.+ integerToPrInt n

instance Pr.Show Integer where
  show Z = "n0"
  show n = "n" Pr.++ Pr.show (integerToPrInt n)

-- instance Pr.Show Integer where
--   show Z = "Z"
--   show (P n) = "P (" Pr.++ Pr.show n Pr.++ ")"
--   show (S n) = "S (" Pr.++ Pr.show n Pr.++ ")"

instance Pr.Show Bool where
  show True = "True"
  show False = "False"
