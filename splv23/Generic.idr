module Generic

import Lib
import Data.Fin
import Data.Vect

import Syntax.PreorderReasoning

%default total

--------------------------------------------------
-- Reminder:

-- :doc Tuple
-- :doc Nat
-- :doc List
-- :doc Vec


--------------------------------------------------
-- Universe of descriptions

data Desc : Type where
  None : Desc
  Byte : Desc
  Pair : Desc -> Desc -> Desc
  Rec  : Desc

%name Desc d, e

Meaning : Desc -> Type -> Type
Meaning None x = Unit
Meaning Byte x = Bits8
Meaning (Pair d e) x = Tuple (Meaning d x) (Meaning e x)
Meaning Rec x = x

LeafArgs : Desc
LeafArgs = None

NodeArgs : Desc
NodeArgs = Pair Rec (Pair Byte Rec)

mkNodeArgs : x -> Bits8 -> x -> Meaning NodeArgs x
mkNodeArgs l b r = l # b # r

-- Desc
-- Meaning

record Constructor where
  constructor (::)
  name : String
  description : Desc

Leaf : Constructor
Leaf = "Leaf" :: LeafArgs

Node : Constructor
Node = "Node" :: NodeArgs

-- Constructor (using (::))
-- Leaf, Node

record Data where
  constructor MkData
  {consNumber : Nat}
  constructors : Vect consNumber Constructor

-- Data
-- Tree

Tree : Data
Tree = MkData [Leaf, Node]



--------------------------------------------------
-- Meaning as Trees

-- Index
-- description

record Index (cs : Data) where
  constructor MkIndex
  getIndex : Fin (consNumber cs)


-- /!\ Injectivity, unification, reconstruction

description : {cs : Data} -> Index cs -> Desc
description k
  = description
  $ index (getIndex k) (constructors cs)


Alg : Data -> Type -> Type
Alg cs x
  = (k : Index cs) ->
    Meaning (description k) x ->
    x

-- Algebras
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

leafCons : Index Tree
leafCons = "Leaf"

failing "Can't find an implementation"
  consCons : Index Tree
  consCons = "Cons"


-- leaf, node

leaf : Mu Tree
leaf = "Leaf" # ()


node : Mu Tree -> Bits8 -> Mu Tree -> Mu Tree
node l b r = "Node" # l # b # r

example : Mu Tree
example = node (node (node leaf 1 leaf) 5 leaf) 10 (node leaf 20 leaf)



--------------------------------------------------
-- Generic programs

fmap : (d : Desc) -> (x -> y) ->
       Meaning d x -> Meaning d y
fmap None f v = v
fmap Byte f v = v
fmap (Pair d e) f (v # w)
  = (fmap d f v # fmap e f w)
fmap Rec f v = f v

-- fmap
-- fold

fold : {cs : Data} -> Alg cs x ->
       Mu cs -> x
fold alg t
  = let (k # v) = t in
    let rec = fmap (description k) (assert_total (fold alg)) v in
    alg k rec

-- /!\ Termination checking

-- safe fold
-- /!\ Manual supercompilation (if time allows)



safeFold : {cs : Data} -> Alg cs x -> Mu cs -> x
fmapFold : {cs : Data} -> (d : Desc) -> Alg cs x ->
           Meaning d (Mu cs) -> Meaning d x

safeFold alg t
  = let (k # v) = t in
    let rec = fmapFold (description k) alg v in
    alg k rec


fmapFold None alg v = v
fmapFold Byte alg v = v
fmapFold (Pair d e) alg (v # w)
  = (fmapFold d alg v # fmapFold e alg w)
fmapFold Rec alg v = safeFold alg v


sum : Mu Tree -> Nat
sum = fold $ \case
  "Leaf" => \ _ => 0
  "Node" => \ (m # b # n) =>
    m + cast b + n

test : Nat
test = sum example



--------------------------------------------------
-- Generic proofs

-- All

All : (d : Desc) -> (p : x -> Type) ->
      Meaning d x -> Type
All None p v = ()
All Byte p v = ()
All (Pair d e) p (v # w)
  = Tuple (All d p v) (All e p w)
All Rec p v = p v

Any : (d : Desc) -> (p : x -> Type) ->
      Meaning d x -> Type
Any None p v = Void
Any Byte p v = Void
Any (Pair d e) p (v # w)
  = Either (Any d p v) (Any e p w)
Any Rec p v = p v

-- any : (d : Desc) -> Any d p v -> (y : x ** p y)

test1 : All NodeArgs
        (\ (k # t) => k === "Leaf")
        (Generic.leaf # 10 # node Generic.leaf 20 Generic.leaf)
test1 = ?aglj

-- /!\ Every reasonable 'holey'-thing has an All and an Any

InductionStep : (cs : Data) -> (p : Mu cs -> Type) -> Type
InductionStep cs p
  = (k : Index cs) -> (v : Meaning (description k) (Mu cs)) ->
    All (description k) p v ->
    p (k # v)

-- InductionStep

all : (d : Desc) ->
      {0 p : x -> Type} ->
      (prf : (v : x) -> p v) ->
      (v : Meaning d x) ->
      All d p v
all None prf v = v
all Byte prf v = ()
all (Pair d e) prf (v # w)
  = all d prf v # all e prf w
all Rec prf v = prf v

-- induction
induction : {cs : Data} ->
            {0 p : Mu cs -> Type} ->
            InductionStep cs p ->
            (t : Mu cs) -> p t
induction step (k # t)
  = step k t
  $ assert_total
  $ all (description k) (induction step) t

mirror : Mu Tree -> Mu Tree
mirror = fold $ \case
  "Leaf" => \ _ => leaf
  "Node" => \ (l # b # r) => node r b l


aux : (m, n, p : Nat) ->
      m + n + p === p + n + m
aux m n p = Calc $
  |~ m + n + p
  ~~ n + m + p ...( cong (+ p) (plusCommutative m n) )
  ~~ p + (n + m) ...( plusCommutative (n + m) p )
  ~~ p + n + m ...( plusAssociative p n m )

sumMirror : (t : Mu Tree) ->
            sum t === sum (mirror t)
sumMirror = induction $ \case
  "Leaf" => \v, x => Refl
  "Node" => \ (l # b # r), (leq # () # req) => Calc $
    |~ sum l + cast b + sum r
    ~~ sum (mirror l) + cast b + sum (mirror r)
       ...( cong2 (\ m, n => m + cast b + n) leq req)
    ~~ sum (mirror r) + cast b + sum (mirror l)
       ...( aux (sum (mirror l)) (cast b) (sum (mirror r)) )
