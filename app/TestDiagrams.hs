{-# LANGUAGE QuasiQuotes #-}
import Diagrams.Backend.SVG.CmdLine

import Data.String.Here.Uninterpolated (here)

import Patchwork.Parser
import Patchwork.Diagram

main :: IO ()
main = let solution = parse exercice2
           diagM = getDiagram =<< solution
       in do
  case diagM of
    Nothing -> putStrLn "Error in parsing / execution"
    Just diag -> mainWith diag

-- tests

exercice2 :: String
exercice2 = [here|
def losange = rot (b+rot rot rot b)
            + rot (rot b+rot rot b);
def ligne = losange [3];
def damier = (rot ligne) [3];
show damier;
|]
