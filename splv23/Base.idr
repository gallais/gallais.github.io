module Base

%default total

------------------------------------------------------------------------
-- Basic Functions
------------------------------------------------------------------------

id : a -> a
id = \ x => x

-- /!\ Implicit Prenex Polymorphism
-- /!\ Quantities

infixr 3 .

(.) : (b -> c) -> (a -> b) -> a -> c
(g . f) x = g (f x)

------------------------------------------------------------------------
-- Base Types
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Empty type

data Void : Type where

Not : Type -> Type
Not a = a -> Void

-- /!\ Types Are Terms

doubleNeg : a -> Not (Not a)
doubleNeg x = \ f => f x

------------------------------------------------------------------------
-- Boring type

record Unit where
  constructor MkUnit

------------------------------------------------------------------------
-- Boolean type

data Bool = True | False

not : Bool -> Bool
not True = False
not False = True

-- /!\ Total Functions

and : Bool -> Bool -> Bool
and False _ = False
and _ b = b

is : Type -> Bool -> Type
a `is` True = a
a `is` False = Not a

-- /!\ Large Elimination

UnitIsTrue : Unit `is` True
UnitIsTrue = ()

VoidIsFalse : Void `is` False
VoidIsFalse = \ x => x

NotIsNot : (b : Bool) -> a `is` b -> Not a `is` not b
NotIsNot True = doubleNeg
NotIsNot False = \ x => x

-- /!\ Arbitrary Computations at Typechecking Time

------------------------------------------------------------------------
-- Pairing

infixr 3 #

namespace PRINTINGONLY
  data Tuple : Type -> Type -> Type where
    (#) : a -> b -> Tuple a b

  fst : Tuple a b -> a
  fst (x # y) = x

  snd : Tuple a b -> b
  snd (x # y) = y

record Tuple (a, b : Type) where
  constructor (#)
  fst : a
  snd : b

-- /!\ Records are Datatypes

------------------------------------------------------------------------
-- Sum

data Either : Type -> Type -> Type where
  Left : a -> Either a b
  Right : b -> Either a b

------------------------------------------------------------------------
-- Nat

data Nat = Z | S Nat

-- /!\ Runtime Optimisation of Types

infixl 8 +

(+) : Nat -> Nat -> Nat
Z + n = n
S m + n = S (m + n)

------------------------------------------------------------------------
-- List

infixr 3 ::

data List : Type -> Type where
  Nil : List a
  (::) : a -> List a -> List a

map : (a -> b) -> List a -> List b
map f [] = []
map f (x :: xs) = f x :: map f xs

infixr 7 ++

(++) : List a -> List a -> List a
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

------------------------------------------------------------------------
-- Indexed families
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Vec

namespace Vec

  public export
  data Vec : Nat -> Type -> Type where
    Nil : Vec Z a
    (::) : a -> Vec n a -> Vec (S n) a

  map : (a -> b) -> Vec n a -> Vec n b
  map f [] = []
  map f (x :: xs) = f x :: map f xs


  (++) : Vec m a -> Vec n a -> Vec (m + n) a
  [] ++ ys = ys
  (x :: xs) ++ ys = x :: (xs ++ ys)


------------------------------------------------------------------------
-- Fin

namespace Fin

  public export
  data Fin : Nat -> Type where
    Z : Fin (S n)
    S : Fin n -> Fin (S n)

fin0 : Not (Fin Z)
fin0 k impossible

-- /!\ Empty Case Tree

lookup : Vec n a -> Fin n -> a
lookup (x :: xs) Z = x
lookup (x :: xs) (S k) = lookup xs k

-- /!\ Dependent Pattern-Matching

------------------------------------------------------------------------
-- Equality type

infix 0 ===

data (===) : {a : Type} -> (x : a) -> a -> Type where
  Refl : x === x

cong : (f : a -> b) -> x === y -> f x === f y
cong f Refl = Refl

------------------------------------------------------------------------
-- Eta rules

-- Boring type is boring
etaUnit : (x, y : Unit) -> x === y
etaUnit MkUnit MkUnit = Refl

etaTuple : (p : Tuple a b) -> p === (fst p # snd p)
etaTuple (x # y) = Refl

------------------------------------------------------------------------
-- Proofs by case analysis

-- not is involutive
notInvolutive : (b : Bool) -> not (not b) === b
notInvolutive True = Refl
notInvolutive False = Refl

-- and is associative
andAssociative : (a, b, c : Bool) ->
  and a (and b c) === and (and a b) c
andAssociative False _ _ = Refl
andAssociative True _ _ = Refl

------------------------------------------------------------------------
-- Proof by induction

-- list fusion
mapFusion : (g : b -> c) -> (f : a -> b) -> (xs : List a) ->
  map g (map f xs) === map (g . f) xs
mapFusion g f [] = Refl
mapFusion g f (x :: xs) = cong (g (f x) ::) (mapFusion g f xs)
