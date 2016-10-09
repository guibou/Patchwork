module Patchwork.Ast where

data Instruction = Show Draw
                 | Size Draw
                 | Def Ident Draw
                 deriving (Show)

data Ident = Ident String deriving (Show, Ord, Eq)

data Draw = Repeat Draw Int
          | Rotate Draw
          | Prim PrimID
          | Id Ident
          | Concat Draw Draw deriving (Show)

data PrimID = A | B | C | D deriving (Show)
