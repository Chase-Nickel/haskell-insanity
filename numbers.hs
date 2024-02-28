{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- for stdio only
import Prelude qualified as P

nil = P.undefined

data Boolean = True | False

boolToBoolean :: P.Bool -> Boolean
boolToBoolean P.True = True
boolToBoolean P.False = False

not :: Boolean -> Boolean
not True = False
not False = True

and :: Boolean -> Boolean -> Boolean
and True True = True
and _ _ = False

nand :: Boolean -> Boolean -> Boolean
nand b1 b2 = not (and b1 b2)

or :: Boolean -> Boolean -> Boolean
or True _ = True
or _ True = True
or False False = False

nor :: Boolean -> Boolean -> Boolean
nor b1 b2 = not (or b1 b2)

xor :: Boolean -> Boolean -> Boolean
xor True True = False
xor False True = True
xor True False = True
xor False False = False

xnor :: Boolean -> Boolean -> Boolean
xnor b1 b2 = not (xor b1 b2)

if' :: Boolean -> a -> a -> a
if' True x _ = x
if' False _ y = y

data Peano = Z | S Peano

u :: P.Int -> Peano
u 0 = Z
u a = S (u (a P.- 1))

peanoEQ :: Peano -> Peano -> Boolean
peanoEQ Z Z = True
peanoEQ Z (S b) = False
peanoEQ (S a) Z = False
peanoEQ (S a) (S b) = peanoEQ a b

peanoLT :: Peano -> Peano -> Boolean
peanoLT Z Z = False
peanoLT Z (S b) = True
peanoLT (S a) Z = False
peanoLT (S a) (S b) = peanoLT a b

peanoLTE :: Peano -> Peano -> Boolean
peanoLTE a b = or (peanoEQ a b) (peanoLT a b)

peanoGT :: Peano -> Peano -> Boolean
peanoGT Z Z = False
peanoGT Z (S b) = False
peanoGT (S a) Z = True
peanoGT (S a) (S b) = peanoGT a b

peanoGTE :: Peano -> Peano -> Boolean
peanoGTE a b = or (peanoEQ a b) (peanoGT a b)

peanoAdd :: Peano -> Peano -> Peano
peanoAdd Z b = b
peanoAdd a Z = a
peanoAdd (S a) b = S (peanoAdd a b)

peanoAbsSub :: Peano -> Peano -> Peano
peanoAbsSub Z Z = Z
peanoAbsSub Z b = b
peanoAbsSub a Z = a
peanoAbsSub (S a) (S b) = peanoAbsSub a b

peanoSub :: Peano -> Peano -> Peano
peanoSub Z Z = Z
peanoSub Z b = nil
peanoSub a Z = a
peanoSub (S a) (S b) = peanoSub a b

peanoMul :: Peano -> Peano -> Peano
peanoMul Z _ = Z
peanoMul _ Z = Z
peanoMul (S Z) (S b) = S b
peanoMul (S a) (S Z) = S a
peanoMul a (S b) = peanoAdd a (peanoMul a b)

peanoFloorDiv :: Peano -> Peano -> Peano
peanoFloorDiv Z Z = nil
peanoFloorDiv Z _ = Z
peanoFloorDiv a b =
  if'
    (peanoGTE a b)
    (peanoAdd (S Z) (peanoFloorDiv (peanoSub a b) b))
    Z

peanoModulus :: Peano -> Peano -> Peano
peanoModulus a Z = nil
peanoModulus a b =
  if'
    (peanoGTE a b)
    (peanoSub a (peanoMul (peanoFloorDiv a b) b))
    a

data List a = Nil | Cons a (List a)

index :: List a -> Peano -> a
index (Cons a b) Z = a
index (Cons a b) (S c) = index b c

-- first element in a list
first :: List a -> a
first (Cons a b) = a

-- all elements but the first in the list
tail :: List a -> List a
tail (Cons a b) = b

concat :: List t -> List t -> List t
concat a Nil = a
concat Nil b = b
concat (Cons a b) c = Cons a (concat b c)

appendEnd :: List t -> t -> List t
appendEnd Nil b = Cons b Nil
appendEnd (Cons a b) c = Cons a (appendEnd b c)

appendStart :: List t -> t -> List t
appendStart Nil b = Cons b Nil
appendStart a c = concat (Cons c Nil) a

-- reverse a list
rev :: List t -> List t
rev Nil = Nil
rev (Cons a Nil) = Cons a Nil
rev (Cons a b) = concat (rev b) (Cons a Nil)

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)

-- list of numbers from end (exclusive) to zero
range :: Peano -> List Peano
range Z = Nil
range (S a) = Cons a (range a)

length :: List t -> Peano
length Nil = Z
length (Cons a b) = peanoAdd (length b) (S Z)

data Integer = Integer Peano Peano

i :: P.Int -> Integer
i 0 = Integer Z Z
i a =
  if'
    (boolToBoolean (a P.< 0))
    (Integer Z (u (-a)))
    (Integer (u a) Z)

booleanToInteger :: Boolean -> Integer
booleanToInteger True = Integer (S Z) Z
booleanToInteger False = Integer Z Z

integerAdd :: Integer -> Integer -> Integer
integerAdd (Integer a b) (Integer c d) = Integer (peanoAdd a c) (peanoAdd b d)

integerNeg :: Integer -> Integer
integerNeg (Integer a b) = Integer b a

integerSub :: Integer -> Integer -> Integer
integerSub (Integer a b) (Integer c d) = Integer (peanoAdd a d) (peanoAdd b c)

integerMul :: Integer -> Integer -> Integer
integerMul (Integer a b) (Integer c d) =
  Integer
    (peanoAdd (peanoMul a c) (peanoMul b d))
    (peanoAdd (peanoMul a d) (peanoMul b c))

integerAbs :: Integer -> Integer
integerAbs (Integer a b) = Integer (peanoAbsSub a b) Z

integerEQ :: Integer -> Integer -> Boolean
integerEQ (Integer a b) (Integer c d) = peanoEQ (peanoAdd a d) (peanoAdd b c)

integerLT :: Integer -> Integer -> Boolean
integerLT (Integer a b) (Integer c d) = peanoLT (peanoAdd a d) (peanoAdd b c)

integerLTE :: Integer -> Integer -> Boolean
integerLTE a b = or (integerLT a b) (integerEQ a b)

integerGT :: Integer -> Integer -> Boolean
integerGT (Integer a b) (Integer c d) = peanoGT (peanoAdd a d) (peanoAdd b c)

integerGTE :: Integer -> Integer -> Boolean
integerGTE a b = or (integerGT a b) (integerEQ a b)

integerIsZero :: Integer -> Boolean
integerIsZero (Integer a b) = peanoEQ a b

integerIsPos :: Integer -> Boolean
integerIsPos (Integer a b) = peanoGT a b

integerIsNeg :: Integer -> Boolean
integerIsNeg (Integer a b) = peanoLT a b

integerPosMag :: Integer -> Peano
integerPosMag (Integer a _) = a

integerNegMag :: Integer -> Peano
integerNegMag (Integer _ b) = b

integerSgn :: Integer -> Integer
integerSgn a =
  Integer
    (integerPosMag (booleanToInteger (integerIsPos a)))
    (integerPosMag (booleanToInteger (integerIsNeg a)))

integerToPeano :: Integer -> Peano
integerToPeano (Integer a b) =
  if'
    (not (integerIsNeg (Integer a b)))
    (peanoAbsSub a b)
    nil

integerFloorDiv :: Integer -> Integer -> Integer
integerFloorDiv a b =
  if'
    (not (integerIsZero b))
    ( integerMul
        ( integerMul
            (integerSgn a)
            (integerSgn b)
        )
        ( Integer
            ( peanoFloorDiv
                (integerToPeano a)
                (integerToPeano b)
            )
            Z
        )
    )
    nil

integerMod :: Integer -> Integer -> Integer
integerMod a b = integerSub a (integerMul (integerFloorDiv a b) b)

integerRemainder :: Integer -> Integer -> Integer
integerRemainder a b = nil

(+) = integerAdd

(-) = integerSub

(*) = integerMul

(//) = integerFloorDiv

(==) = integerEQ

(<) = integerLT

(<=) = integerLTE

(>) = integerGT

(>=) = integerGTE

data Rational = Rational Integer Integer

-- stdio

peanoToInt :: Peano -> P.Int
peanoToInt Z = 0
peanoToInt (S n) = 1 P.+ peanoToInt n

instance P.Show Peano where
  show Z = "n0"
  show a = "n" P.++ P.show (peanoToInt a)

instance P.Show Boolean where
  show True = "True"
  show False = "False"

instance P.Show Integer where
  show (Integer a b) = "i" P.++ P.show (peanoToInt a P.- peanoToInt b)

instance (P.Show t) => P.Show (List t) where
  show (Cons a Nil) = P.show a
  show (Cons a b) = P.show a P.++ ", " P.++ P.show b
  show Nil = "nil"

