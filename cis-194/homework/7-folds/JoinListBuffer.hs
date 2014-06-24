{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid

import JoinList
import Scrabble
import Sized
import Buffer

instance Buffer (JoinList (Score, Size) String) where
  toString buffer = unlines (toList buffer)
  fromString s =
    foldr (+++) Empty (map f (lines s))
    where
      f k = Single (scoreString k, 1) k
  line n = indexJ (n+1) 
  replaceLine lineNumber newLine buffer =
    (takeJ (lineNumber-1) buffer) +++ fromString newLine +++ (dropJ lineNumber buffer)
  numLines = getSize . snd . tag
  value = getScore . fst . tag
