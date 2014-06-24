module Main where

import Data.Monoid

import StringBuffer
import JoinListBuffer
import Editor
import Buffer
import JoinList
import Scrabble
import Sized

main = runEditor editor $ jlBuffer

jlBuffer :: JoinList (Score, Size) String
jlBuffer = fromString "stuffs"

jlBuffer2 :: JoinList (Score, Size) String
jlBuffer2 = fromString (unlines text)

sbuffer = unlines text

text = [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
