{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- for stdio only
import Prelude qualified as P (Int, Show, show, (+), (++), (-))

data Bool = True | False

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

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

data Peano = Z | S Peano | NaN | Infinity

-- for the losers who can't type S (S (S (S (S (S (S (S (Z)))))))) to get 8
u :: P.Int -> Peano
u 0 = Z
u a = S (u (a P.- 1))

peanoEQ :: Peano -> Peano -> Bool
peanoEQ Z Z = True
peanoEQ Z (S b) = False
peanoEQ (S a) Z = False
peanoEQ (S a) (S b) = peanoEQ a b

peanoLT :: Peano -> Peano -> Bool
peanoLT Z Z = False
peanoLT Z (S b) = True
peanoLT (S a) Z = False
peanoLT (S a) (S b) = peanoLT a b

peanoLTE :: Peano -> Peano -> Bool
peanoLTE a b = or (peanoEQ a b) (peanoLT a b)

peanoGT :: Peano -> Peano -> Bool
peanoGT Z Z = False
peanoGT Z (S b) = False
peanoGT (S a) Z = True
peanoGT (S a) (S b) = peanoGT a b

peanoGTE :: Peano -> Peano -> Bool
peanoGTE a b = or (peanoEQ a b) (peanoGT a b)

peanoAdd :: Peano -> Peano -> Peano
peanoAdd NaN _ = NaN
peanoAdd _ NaN = NaN
peanoAdd _ Infinity = Infinity
peanoAdd Infinity _ = Infinity
peanoAdd Z b = b
peanoAdd a Z = a
peanoAdd (S a) b = S (peanoAdd a b)

peanoAbsSub :: Peano -> Peano -> Peano
peanoAbsSub NaN _ = NaN
peanoAbsSub _ NaN = NaN
peanoAbsSub _ Infinity = Infinity
peanoAbsSub Infinity _ = Infinity
peanoAbsSub Z Z = Z
peanoAbsSub Z b = b
peanoAbsSub a Z = a
peanoAbsSub (S a) (S b) = peanoAbsSub a b

peanoSub :: Peano -> Peano -> Peano
peanoSub NaN _ = NaN
peanoSub _ NaN = NaN
peanoSub _ Infinity = NaN
peanoSub Infinity _ = Infinity
peanoSub Z Z = Z
peanoSub Z b = NaN
peanoSub a Z = a
peanoSub (S a) (S b) = peanoSub a b

peanoMul :: Peano -> Peano -> Peano
peanoMul NaN _ = NaN
peanoMul _ NaN = NaN
peanoMul Infinity _ = Infinity
peanoMul _ Infinity = Infinity
peanoMul Z _ = Z
peanoMul _ Z = Z
peanoMul (S Z) (S b) = S b
peanoMul (S a) (S Z) = S a
peanoMul a (S b) = peanoAdd a (peanoMul a b)

peanoFloorDiv :: Peano -> Peano -> Peano
peanoFloorDiv NaN _ = NaN
peanoFloorDiv _ NaN = NaN
peanoFloorDiv Infinity Infinity = NaN
peanoFloorDiv Infinity _ = Infinity
peanoFloorDiv _ Infinity = Z
peanoFloorDiv Z Z = NaN
peanoFloorDiv Z _ = Z
peanoFloorDiv _ Z = Infinity
peanoFloorDiv a b =
  if'
    (peanoGTE a b)
    (peanoAdd (S Z) (peanoFloorDiv (peanoSub a b) b))
    Z

peanoModulus :: Peano -> Peano -> Peano
peanoModulus NaN _ = NaN
peanoModulus _ NaN = NaN
peanoModulus Infinity _ = NaN
peanoModulus _ Infinity = NaN
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

(+) = peanoAdd

(-) = peanoAbsSub

(*) = peanoMul

(/) = peanoFloorDiv

(%) = peanoModulus

(==) = peanoEQ

(<) = peanoLT

(>) = peanoGT

(<=) = peanoLTE

(>=) = peanoGTE

-- stdio

peanoToInt :: Peano -> P.Int
peanoToInt Z = 0
peanoToInt (S n) = 1 P.+ peanoToInt n

instance P.Show Peano where
  show Z = "n0"
  show NaN = "NaN"
  show Infinity = "Infty"
  show (S n) = "n" P.++ P.show (peanoToInt (S n))

instance P.Show Bool where
  show True = "True"
  show False = "False"

instance (P.Show t) => P.Show (List t) where
  show (Cons a Nil) = P.show a
  show (Cons a b) = P.show a P.++ ", " P.++ P.show b
  show Nil = "nil"
