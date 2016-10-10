{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts  #-}
module Patchwork.Parser where

import Data.Char (isAlpha, isDigit, isSpace)

import Control.Applicative
import qualified Text.Earley as E

import Patchwork.Ast

grammar :: forall r. E.Grammar r (E.Prod r String Char [Instruction])
grammar = mdo
  prog <- E.rule $
    many (instruction <* sym ";")

  instruction <- E.rule $
        sym "show" *> (Show <$> draw)
    <|> sym "size" *> (Size <$> draw)
    <|> sym "def" *> (Def <$> (ident <* sym "=") <*> draw)

  draw <- E.rule $
        Id <$> ident
    <|> sym "(" *> draw <* sym ")"
    <|> Concat <$> draw <*> (sym "+" *> draw)
    <|> Rotate <$> (sym "rot" *> draw)
    <|> Repeat <$> draw <*> (sym "[" *> int <* sym "]")

  -- lexing
  let tok p   = (many $ E.satisfy isSpace) *> p
      sym x   = tok $ E.list x

      ident = Ident <$> tok (many (E.satisfy isAlpha))
      int     = read <$> (tok $ some (E.satisfy isDigit))

  return prog

parse :: String -> Maybe [Instruction]
parse s = case fst (E.fullParses (E.parser grammar) s) of

               [] -> Nothing
               (x:_) -> Just x
