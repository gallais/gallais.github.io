module Generic

import Lib
import Data.Fin
import Data.Vect

%default total

--------------------------------------------------
-- Reminder:

-- :doc Tuple
-- :doc Nat
-- :doc List
-- :doc Vec


--------------------------------------------------
-- Universe of descriptions

-- Desc

data Desc : Type where
  None : Desc
  Byte : Desc
  Pair : (d, e : Desc) -> Desc
  Rec : Desc

-- Meaning

Meaning : Desc -> Type -> Type
Meaning None x = ()
Meaning Byte x = Bits8
Meaning (Pair d e) x = Tuple (Meaning d x) (Meaning e x)
Meaning Rec x = x

-- Meaning

-- Constructor (using (::))

record Constructor where
  constructor (::)
  name : String
  description : Desc

-- Leaf, Node

Leaf : Constructor
Leaf = "Leaf" :: None

Node : Constructor
Node = "Node" :: Pair Rec (Pair Byte Rec)

-- Data

record Data where
  constructor MkData
  {consNumber : Nat}
  constructors : Vect consNumber Constructor

-- Tree

Tree : Data
Tree = MkData [Leaf, Node]


--------------------------------------------------
-- Meaning as Trees

-- Index

record Index (cs : Data) where
  constructor MkIndex
  getIndex : Fin (consNumber cs)

-- description

description : {cs : Data} -> Index cs -> Desc
description (MkIndex k) = description (index k (constructors cs))

-- /!\ Injectivity, unification, reconstruction

-- Algebras

Alg : Data -> Type -> Type
Alg cs x = (k : Index cs) -> Meaning (description k) x -> x

-- Mu

data Mu : Data -> Type where
  (#) : Alg cs (assert_total (Mu cs))

-- /!\ Strict positivity


-- MAGIC

isConstructor : String -> (cs : Data) -> Maybe (Fin cs.consNumber)
isConstructor x cs = findIndex ((x ==) . name) (constructors cs)

-- used to desugar string literals

fromString : {cs : Data} -> (str : String) ->
             {auto 0 _ : IsJust (isConstructor str cs)} ->
             Index cs
fromString {cs} str with (isConstructor str cs)
 _ | Just k = MkIndex k

-- valid & invalid Index Tree

namespace Example

  leaf : Index Tree
  leaf = "Leaf"

  failing

    cons : Index Tree
    cons = "Cons" -- should ne "Node"


-- leaf, node

leaf : Mu Tree
leaf = "Leaf" # ()

node : Mu Tree -> Bits8 -> Mu Tree -> Mu Tree
node l b r = "Node" # l # b # r

example : Mu Tree
example = node (node (node leaf 1 leaf) 5 leaf) 10 (node leaf 20 leaf)


--------------------------------------------------
-- Generic programs

-- fmap

fmap : (d : Desc) -> (x -> y) -> Meaning d x -> Meaning d y
fmap None f v = v
fmap Byte f v = v
fmap (Pair d e) f (v # w) = (fmap d f v # fmap e f w)
fmap Rec f v = f v

-- fold
-- /!\ Termination checking

fold : {cs : Data} -> Alg cs a -> Mu cs -> a
fold alg t
  = let (k # v) = t in
    let rec = assert_total (fmap _ (fold alg) v) in
    alg k rec

-- safe fold
-- /!\ Manual supercompilation (if time allows)

mutual

  safefold : {cs : Data} -> Alg cs a -> Mu cs -> a
  safefold alg (k # v) = alg k (fmapfold alg _ v)

  fmapfold : {cs : Data} -> Alg cs a -> (d : Desc) ->
             Meaning d (Mu cs) -> Meaning d a
  fmapfold alg None v = v
  fmapfold alg Byte v = v
  fmapfold alg (Pair d e) (v # w) = (fmapfold alg d v # fmapfold alg e w)
  fmapfold alg Rec v = safefold alg v

--------------------------------------------------
-- Generic proofs

-- All
-- /!\ Every reasonable 'holey'-thing has an All and an Any

All : (d : Desc) -> (x -> Type) -> Meaning d x -> Type
All None p v = ()
All Byte p v = ()
All (Pair d e) p (v # w) = (All d p v, All e p w)
All Rec p v = p v

-- InductionStep

InductionStep : {cs : Data} -> (p : Mu cs -> Type) -> Type
InductionStep p
  = (k : Index cs) -> (v : Meaning (description k) (Mu cs)) ->
    All (description k) p v -> p (k # v)

-- induction

all : (p : x -> Type) -> (f : (v : x) -> p v) ->
  (d : Desc) -> (v : Meaning d x) -> All d p v
all p f None v = ()
all p f Byte v = ()
all p f (Pair d e) (v # w) = (all p f d v, all p f e w)
all p f Rec v = f v

induction : {cs : Data} -> (p : Mu cs -> Type) ->
  (step : InductionStep p) -> (t : Mu cs) -> p t
induction p step (k # v)
  = step k v $ assert_total
  $ all p (induction p step) (description k) v
