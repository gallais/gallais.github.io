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
-- Meaning

-- Constructor (using (::))
-- Leaf, Node

-- Data
-- Tree






--------------------------------------------------
-- Meaning as Trees

-- Index
-- description

-- /!\ Injectivity, unification, reconstruction

-- Algebras
-- Mu

-- /!\ Strict positivity










-- MAGIC

-- isConstructor : String -> (cs : Data) -> Maybe (Fin cs.consNumber)
-- isConstructor x cs = findIndex ((x ==) . name) (constructors cs)

-- fromString : {cs : Data} -> (str : String) ->
--              {auto 0 _ : IsJust (isConstructor str cs)} ->
--              Index cs
-- fromString {cs} str with (isConstructor str cs)
--  _ | Just k = MkIndex k


-- valid & invalid Index Tree









-- leaf
-- node

-- example : Mu Tree
-- example = node (node (node leaf 1 leaf) 5 leaf) 10 (node leaf 20 leaf)








--------------------------------------------------
-- Generic programs


-- fmap
-- fold

-- /!\ Termination checking

-- safe fold


-- /!\ Manual supercompilation (if time allows)





--------------------------------------------------
-- Generic proofs


-- All



-- /!\ Every reasonable 'holey'-thing has an All and an Any



-- InductionStep

-- induction
