module Utils.Types where

data Nat = Zero | Succ Nat

data TreeList = TNil | TCons Tree TreeList
data NatList = NNil | NCons Nat NatList

data Tree = Node (Tree) Nat (Tree) | Niil 


