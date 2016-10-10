module Patchwork.Ast where

data Instruction = Show Draw
                 | Size Draw
                 | Def Ident Draw
                 deriving (Show)

data Ident = Ident String deriving (Show, Ord, Eq)

data Draw = Repeat Draw Int
          | Rotate Draw
          | Id Ident
          | Concat Draw Draw deriving (Show)
