module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int deriving(Show)

instance Monoid Score where
  mempty = Score 0
  mappend (Score a) (Score b) = Score (a+b)

score :: Char -> Score
score c
  | celem "AEILNORSTU" = Score 1
  | celem "DG" = Score 2
  | celem "BCMP" = Score 3
  | celem "FHVWY" = Score 4
  | celem "K" = Score 5
  | celem "JX" = Score 8
  | celem "QZ" = Score 10
  | otherwise = Score 0
  where celem = elem (toUpper c)
    
getScore :: Score -> Int
getScore (Score i) = i

scoreString :: String -> Score
scoreString s = foldr mappend mempty scores
  where scores = map score s
