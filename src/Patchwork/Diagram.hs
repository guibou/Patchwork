{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, QuasiQuotes #-}
module Patchwork.Diagram where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Patchwork.Ast
import qualified Data.Map as Map

-- The four initial diagrams
diagA :: Diagram B
diagA = (arc xDir (90 @@ deg) <> fromVertices [p2 (0, 0), p2 (1, 0)])# closeTrail # strokeTrail # fc black # translateY (-0.5) # translateX (0.5) # lw none

diagB :: Diagram B
diagB = fromVertices vertices # closeTrail # strokeTrail # fc black # translateY (0.5) # translateX (0.5) # lw none
  where vertices = map p2 [(1, 1), (1, 0), (0, 1)]

diagC :: Diagram B
diagC = square 1 # fc black # lw none

diagD :: Diagram B
diagD = square 1 # fc white # lw none

-- Rotate a diagram
rot :: Diagram B -> Diagram B
rot = rotate ((-90) @@ deg)

-- Repeat n times a diagram
rep :: Int -> Diagram B -> Diagram B
rep 1 d = d
rep n d = d ||| (rep (n-1) d)

-- evaluate a list of instruction to a diagram. returns `Nothing` on failure
getDiagram :: [Instruction] -> Maybe (Diagram B)
getDiagram instr = go (Map.empty) instr -- the map stores the current saved diagrams
  where
    go _ [] = Nothing
    go m (x:xs) = case x of
      (Size _) -> Nothing -- I don't know what I'm supposed to do with the `Size` instruction
      (Show d) -> evalDraw m d -- Evaluate the diagram
      (Def name d) -> do -- propagate the error during the evaluation of the dram
        res <- evalDraw m d
        let m' = Map.insert name res m
        go m' xs

-- diagram evaluation, using a map of evaluated diagrams
evalDraw :: Map.Map Ident (Diagram B) -> Draw -> Maybe (Diagram B)
evalDraw m (Repeat d n) = rep n <$> (evalDraw m d)
evalDraw m (Rotate d) = rot <$> evalDraw m d
evalDraw _ (Patchwork.Ast.Prim p) = Just $ case p of
  A -> diagA
  B -> diagB
  C -> diagC
  D -> diagD
evalDraw m (Id name) = Map.lookup name m
evalDraw m (Concat a b) = liftA2 (|||) (evalDraw m a) (evalDraw m b)
